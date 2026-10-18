defmodule Vaisto.Elab.Classes do
  @moduledoc false
  # Surface classes disappear into ordinary typed Core dictionary factories.
  alias Vaisto.Elab.Types
  alias Vaisto.{Error, Parser.Loc}

  def expand(forms) do
    Enum.flat_map(forms, fn
      {:deftype_deriving, name, body, classes, loc} ->
        [{:deftype, name, body, loc} | Enum.map(classes, &{:derive, &1, name, body, loc})]

      form ->
        [form]
    end)
  end

  def collect(forms, ctx) do
    arities =
      Map.new(ctx.decls, fn {t, d} -> {t, length(d.params)} end) |> Map.merge(ctx.type_aliases)

    builtins = builtin_classes()
    declared = for {:defclass, c, _, _, _} <- forms, do: c
    forms = Enum.reject(builtins, fn {:defclass, c, _, _, _} -> c in declared end) ++ forms

    classes =
      Enum.reduce(forms, %{}, fn
        {:defclass, c, [a], methods, loc}, acc ->
          if Map.has_key?(acc, c), do: fail(loc, "the class `#{c}` is declared twice")
          if methods == [], do: fail(loc, "a class must declare at least one method")

          methods =
            Enum.map(methods, fn {name, params, ret, default} ->
              type =
                surface({:call, :Fn, Enum.map(params, &elem(&1, 1)) ++ [ret], loc}, arities, loc)

              unless Enum.all?(Types.variables(type), &(&1 == {a, :Type})),
                do: fail(loc, "a method signature uses a type variable outside its class")

              unless a in Types.tvars(type),
                do: fail(loc, "the method `#{name}` does not determine its class parameter")

              {name, type, Enum.map(params, &elem(&1, 0)), default}
            end)

          if methods |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> length() != length(methods),
            do: fail(loc, "a class declares a method twice")

          Map.put(acc, c, %{param: a, methods: methods, loc: loc, builtin?: c not in declared})

        {:defclass, _, _, _, loc}, _ ->
          fail(loc, "a class must have exactly one type parameter")

        _, acc ->
          acc
      end)

    imported = ctx.imports |> Map.values() |> Enum.uniq_by(& &1.module)

    classes =
      Enum.reduce(imported, classes, fn interface, classes ->
        Enum.reduce(interface.classes, classes, fn {c, info}, classes ->
          if Map.has_key?(classes, c),
            do: fail(%Loc{}, "the imported class `#{c}` is already declared")

          methods =
            Enum.map(info.methods, fn {m, type, default} ->
              params = Enum.with_index(Enum.at(type, 1), fn _, i -> :"arg#{i}" end)

              default =
                if default == :none,
                  do: nil,
                  else: {:imported_default, c, default, params, %Loc{}}

              {m, type, params, default}
            end)

          Map.put(classes, c, %{param: info.param, methods: methods, loc: %Loc{}, builtin?: false})
        end)
      end)

    methods =
      Enum.reduce(classes, %{}, fn {c, info}, acc ->
        Enum.reduce(info.methods, acc, fn {name, _, _, _}, acc ->
          if Map.has_key?(acc, name),
            do: fail(info.loc, "the method `#{name}` belongs to two classes")

          Map.put(acc, name, c)
        end)
      end)

    source =
      for form <- forms,
          is_tuple(form),
          elem(form, 0) in [:instance, :instance_constrained, :derive] do
        form =
          case form do
            {:derive, class, name, body, loc} -> derive(class, name, body, loc, ctx.decls)
            form -> form
          end

        instance(form, classes, ctx.decls, arities)
      end

    instances =
      Enum.reduce(source, %{}, fn i, acc ->
        key = {i.class, head(i.type)}

        if Map.has_key?(acc, key),
          do: fail(i.loc, "overlapping instances for #{i.class} #{inspect(i.type)}")

        Map.put(acc, key, i)
      end)

    builtins =
      for form <- builtin_instances(),
          do: Map.put(instance(form, classes, ctx.decls, arities), :builtin?, true)

    instances =
      Enum.reduce(imported, instances, fn interface, instances ->
        Enum.reduce(interface.instances, instances, fn i, instances ->
          key = {i.class, head(i.type)}

          if Map.has_key?(instances, key),
            do: fail(%Loc{}, "overlapping imported instances for #{i.class} #{inspect(i.type)}")

          i = Map.merge(i, %{loc: %Loc{}, remote: interface.module})
          Map.put(instances, key, i)
        end)
      end)

    instances =
      Enum.reduce(builtins, instances, fn i, acc ->
        Map.put_new(acc, {i.class, head(i.type)}, i)
      end)

    defaults =
      for {class, info} <- classes,
          {method, [:->, params, eff, result], names, body} <- info.methods,
          body != nil and not match?({:imported_default, _, _, _, _}, body),
          into: %{} do
        name = :"Class.#{class}.#{method}.default"

        type =
          Types.generalize([
            :->,
            [dictionary_type(classes, class, [:tvar, info.param]) | params],
            eff,
            result
          ])

        {name,
         %{
           class: class,
           param: info.param,
           type: type,
           names: names,
           body: body,
           loc: info.loc,
           builtin?: class not in declared
         }}
      end

    defaults =
      Enum.reduce(imported, defaults, fn interface, defaults ->
        for {_, info} <- interface.classes,
            {_, _, f} <- info.methods,
            f != :none,
            reduce: defaults do
          defaults ->
            [^f, type, :plain] = Enum.find(interface.exports, fn [name, _, _] -> name == f end)

            Map.put(defaults, f, %{
              type: type,
              remote: interface.module,
              loc: %Loc{},
              builtin?: false
            })
        end
      end)

    {:ok,
     %{ctx | classes: classes, methods: methods, instances: instances, class_defaults: defaults}}
  catch
    {:classes, error} -> {:error, [error]}
  end

  def dictionary_type(classes, class, type) do
    info = Map.fetch!(classes, class)

    [
      :Tuple
      | Enum.map(info.methods, fn {_, sig, _, _} ->
          Types.subst_tvars(sig, %{info.param => type})
        end)
    ]
  end

  def builtin_schemas do
    {:ok, ctx} = collect([], struct(Vaisto.Elab))
    ctx.classes
  end

  def export_constraints(ctx) do
    entries =
      for {f, constraints} <- Enum.sort(ctx.function_constraints) do
        [f | Enum.map(constraints, fn {c, t} -> [c, t, dictionary_type(ctx.classes, c, t)] end)]
      end

    classes =
      for {c, info} <- Enum.sort(ctx.classes), not info.builtin? do
        methods =
          Enum.map(info.methods, fn {m, t, _, body} ->
            [m, t, if(body == nil, do: :none, else: :"Class.#{c}.#{m}.default")]
          end)

        [c, info.param, methods]
      end

    instances =
      for {_, i} <- Enum.sort(ctx.instances), not Map.get(i, :builtin?, false) do
        [i.class, i.type, Enum.map(i.constraints, fn {c, t} -> [c, t] end), i.factory]
      end

    if entries == [] and classes == [] and instances == [],
      do: [],
      else: [
        [
          :meta,
          [:"function-constraints" | entries],
          [:classes | classes],
          [:instances | instances]
        ]
      ]
  end

  def head([t | _]), do: t
  def head(t), do: t

  def factory(class, type), do: :"Instance.#{class}.#{head(type)}"

  def prune_builtins(items, instances, defaults) do
    names = for {_, %{builtin?: true, factory: f}} <- instances, do: f
    names = names ++ for {f, %{builtin?: true}} <- defaults, do: f
    roots = Enum.reject(items, fn [:def, f, _, _] -> f in names end)
    reachable(items, names, atoms(roots))
  end

  defp reachable(items, names, used) do
    kept =
      Enum.reject(items, fn [:def, f, _, _] -> f in names and not MapSet.member?(used, f) end)

    next = MapSet.union(used, atoms(kept))
    if next == used, do: kept, else: reachable(items, names, next)
  end

  defp atoms(xs) when is_list(xs), do: Enum.reduce(xs, MapSet.new(), &MapSet.union(atoms(&1), &2))
  defp atoms(x) when is_atom(x), do: MapSet.new([x])
  defp atoms(_), do: MapSet.new()

  defp instance({:instance, c, ann, methods, loc}, classes, decls, arities) do
    type =
      case Map.get(decls, ann) do
        %{params: ps} -> [ann | Enum.map(ps, &[:tvar, &1])]
        _ -> surface(ann, arities, loc)
      end

    make_instance(c, type, [], methods, loc, classes)
  end

  defp instance(
         {:instance_constrained, c, name, params, constraints, methods, loc},
         classes,
         decls,
         arities
       ) do
    unless Map.has_key?(decls, name), do: fail(loc, "unknown instance type `#{name}`")

    unless length(decls[name].params) == length(params) and Enum.uniq(params) == params and
             Enum.all?(params, &is_atom/1),
           do: fail(loc, "invalid instance type parameters")

    constraints =
      Enum.map(constraints, fn {c, annotation} ->
        unless Map.has_key?(classes, c), do: fail(loc, "unknown constraint class `#{c}`")
        type = surface(annotation, arities, loc)

        for a <- Types.tvars(type),
            a not in params,
            do: fail(loc, "constraint variable `#{a}` is not an instance parameter")

        {c, type}
      end)

    unless Enum.uniq(constraints) == constraints, do: fail(loc, "duplicate instance constraint")
    make_instance(c, [name | Enum.map(params, &[:tvar, &1])], constraints, methods, loc, classes)
  end

  defp make_instance(c, type, constraints, methods, loc, classes) do
    info = Map.get(classes, c) || fail(loc, "unknown type class `#{c}`")
    names = Enum.map(methods, &elem(&1, 0))
    unless Enum.uniq(names) == names, do: fail(loc, "an instance defines a method twice")
    known = Enum.map(info.methods, &elem(&1, 0))

    unless names -- known == [],
      do: fail(loc, "an instance defines unknown methods: #{inspect(names -- known)}")

    methods =
      Enum.map(info.methods, fn {name, sig, params, default} ->
        {_, ps, body} =
          List.keyfind(methods, name, 0) ||
            if(default != nil,
              do: {name, params, default},
              else: fail(loc, "missing instance method `#{name}`")
            )

        unless length(ps) == length(Enum.at(sig, 1)),
          do: fail(loc, "wrong arity for instance method `#{name}`")

        body =
          case body do
            {:imported_default, class, f, names, loc} ->
              {:imported_default, class, type, f, names, loc}

            body ->
              body
          end

        {name, Types.subst_tvars(sig, %{info.param => type}), ps, body}
      end)

    %{
      class: c,
      type: type,
      constraints: constraints,
      methods: methods,
      loc: loc,
      factory: factory(c, type)
    }
  end

  defp surface(t, arities, loc) do
    case Types.from_surface(t, arities) do
      {:ok, type} -> type
      {:error, message} -> fail(loc, message)
    end
  end

  defp fail(loc, message),
    do:
      throw(
        {:classes,
         Error.new(message, span: %{line: loc.line, col: loc.col, length: loc.length || 1})}
      )

  defp builtin_classes do
    loc = %Loc{}

    [
      {:defclass, :Eq, [:a],
       [
         {:eq, [x: :a, y: :a], :bool, nil},
         {:neq, [x: :a, y: :a], :bool, {:if, {:call, :eq, [:x, :y], loc}, false, true, loc}}
       ], loc},
      {:defclass, :Show, [:a], [{:show, [x: :a], :string, nil}], loc},
      {:defclass, :Num, [:a],
       [
         {:"num-add", [x: :a, y: :a], :a, nil},
         {:"num-sub", [x: :a, y: :a], :a, nil},
         {:"num-mul", [x: :a, y: :a], :a, nil},
         {:"num-neg", [x: :a], :a, nil},
         {:"num-from-int", [x: :int], :a, nil},
         {:"num-to-float", [x: :a], :float, nil},
         {:"num-round", [x: :a], :int, nil},
         {:"num-trunc", [x: :a], :int, nil}
       ], loc},
      {:defclass, :Ord, [:a],
       [
         {:"ord-lt", [x: :a, y: :a], :bool, nil},
         {:"ord-le", [x: :a, y: :a], :bool, nil},
         {:"ord-gt", [x: :a, y: :a], :bool, nil},
         {:"ord-ge", [x: :a, y: :a], :bool, nil}
       ], loc}
    ]
  end

  defp builtin_instances do
    loc = %Loc{}

    scalar =
      for type <- [:int, :float, :string, :bool, :atom],
          {class, methods} <- [
            Eq: [{:eq, [:x, :y], {:call, :==, [:x, :y], loc}}],
            Show: [{:show, [:x], {:call, :str, [:x], loc}}]
          ],
          do: {:instance, class, type, methods, loc}

    numeric =
      for type <- [:int, :float],
          {class, methods} <- [
            Num: [
              {:"num-add", [:x, :y], {:call, :+, [:x, :y], loc}},
              {:"num-sub", [:x, :y], {:call, :-, [:x, :y], loc}},
              {:"num-mul", [:x, :y], {:call, :*, [:x, :y], loc}},
              {:"num-neg", [:x], {:call, :-, [:x], loc}},
              {:"num-from-int", [:x],
               if(type == :int, do: :x, else: {:call, :"int-to-float", [:x], loc})},
              {:"num-to-float", [:x],
               if(type == :float, do: :x, else: {:call, :"int-to-float", [:x], loc})},
              {:"num-round", [:x],
               if(type == :int, do: :x, else: {:call, :"float-round", [:x], loc})},
              {:"num-trunc", [:x],
               if(type == :int, do: :x, else: {:call, :"float-trunc", [:x], loc})}
            ],
            Ord: [
              {:"ord-lt", [:x, :y], {:call, :<, [:x, :y], loc}},
              {:"ord-le", [:x, :y], {:call, :<=, [:x, :y], loc}},
              {:"ord-gt", [:x, :y], {:call, :>, [:x, :y], loc}},
              {:"ord-ge", [:x, :y], {:call, :>=, [:x, :y], loc}}
            ]
          ],
          do: {:instance, class, type, methods, loc}

    scalar ++ numeric
  end

  defp derive(:Eq, name, _body, loc, decls) do
    %{params: params, kind: kind, members: members} = decls[name]

    if params == [] do
      {:instance, :Eq, name, [{:eq, [:x, :y], {:call, :==, [:x, :y], loc}}], loc}
    else
      variants = if kind == :sum, do: members, else: [{name, Enum.map(members, &elem(&1, 1))}]

      clauses =
        Enum.map(variants, fn {ctor, fields} ->
          xs = Enum.with_index(fields, fn _, i -> :"left#{i}" end)
          ys = Enum.with_index(fields, fn _, i -> :"right#{i}" end)

          equality =
            Enum.zip([xs, ys, fields])
            |> Enum.reduce(true, fn {x, y, type}, acc ->
              op = if Types.tvars(type) == [], do: :==, else: :eq
              {:if, acc, {:call, op, [x, y], loc}, false, loc}
            end)

          {{:tuple_pattern, [{:call, ctor, xs, loc}, {:call, ctor, ys, loc}]}, equality}
        end)

      clauses = if length(variants) > 1, do: clauses ++ [{:_, false}], else: clauses
      body = {:match, {:tuple, [:x, :y], loc}, clauses, loc}
      constraints = Enum.map(params, &{:Eq, &1})
      {:instance_constrained, :Eq, name, params, constraints, [{:eq, [:x, :y], body}], loc}
    end
  end

  defp derive(:Show, name, {:sum, cs}, loc, _decls) do
    unless Enum.all?(cs, fn {_, fs} -> fs == [] end),
      do: fail(loc, "deriving Show requires a fieldless sum")

    clauses = Enum.map(cs, fn {c, []} -> {{:call, c, [], loc}, {:string, Atom.to_string(c)}} end)
    {:instance, :Show, name, [{:show, [:x], {:match, :x, clauses, loc}}], loc}
  end

  defp derive(c, _, _, loc, _decls), do: fail(loc, "cannot derive `#{c}` for this type")
end
