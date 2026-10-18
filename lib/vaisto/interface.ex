defmodule Vaisto.Interface do
  @moduledoc """
  Canonical .vsi interfaces for checked Core modules. Exports, datatype
  declarations, effect rows and class dictionaries are independently
  validated before import; no HM environments or Erlang term files are read.
  """

  alias Vaisto.Liquid.{Canonical, Lint}

  @doc "Build a canonical interface only after independently verifying the Core."
  def from_core(core, sigs \\ %{}, opts \\ []) do
    with :ok <- Vaisto.Refine.check_core(core, sigs, opts) do
      [:module, name, [:"core-version", 0] | items] = Canonical.strip(core)
      exports = for [:def, f, type, _] <- items, do: [f, type, if(Map.has_key?(sigs, f), do: :refined, else: :plain)]
      types = for [:type | _] = type <- items, do: type
      imports = for [:import, m, digest, _] <- items, do: [m, digest]
      info = for [:meta | entries] <- core, [key | values] <- entries,
        key in [:"function-constraints", :classes, :instances], into: %{}, do: {key, values}
      constraints = Map.get(info, :"function-constraints", [])
      tree = [:interface, [:module, name], [:"core-version", 0], [:"ir-version", 1],
        [:exports | Enum.sort(exports)], [:types | Enum.sort(types)], [:imports | Enum.sort(imports)],
        [:status, :verified], [:"core-digest", Canonical.digest(core)], [:constraints | constraints],
        [:classes | Map.get(info, :classes, [])], [:instances | Map.get(info, :instances, [])]]
      with {:ok, _} <- validate_core(tree), do: {:ok, tree}
    end
  end

  @doc "Validate a new interface without decoding Erlang terms or trusting unchecked signatures."
  def validate_core([:interface, [:module, name], [:"core-version", 0], [:"ir-version", 1],
        [:exports | exports], [:types | types], [:imports | imports], [:status, :verified], [:"core-digest", digest], [:constraints | constraints], [:classes | classes], [:instances | instances]] = tree)
      when is_atom(name) and is_binary(digest) and is_list(exports) and is_list(types) and is_list(imports)
        and is_list(constraints) and is_list(classes) and is_list(instances) do
    with true <- byte_size(digest) == 64 and String.match?(digest, ~r/^[0-9a-f]+$/),
         true <- Enum.all?(imports, fn
           [m, d] when is_atom(m) and is_binary(d) -> byte_size(d) == 64 and String.match?(d, ~r/^[0-9a-f]+$/)
           _ -> false
         end),
         true <- Enum.uniq_by(imports, &hd/1) == imports,
         :ok <- Lint.check_module([:module, name, [:"core-version", 0] | types]),
         true <- Enum.all?(exports, &valid_export?(&1, types)),
         names = Enum.map(exports, &hd/1),
         true <- Enum.uniq(names) == names,
         {:ok, classes, schemas} <- validate_classes(classes, exports, types),
         {:ok, constraints} <- validate_constraints(constraints, exports, types, schemas),
         {:ok, instances} <- validate_instances(instances, exports, schemas) do
      {:ok, %{module: name, exports: exports, types: types, imports: imports, constraints: constraints, classes: classes, instances: instances, digest: Canonical.digest(tree), tree: tree}}
    else
      _ -> {:error, Vaisto.Errors.interface_invalid_format()}
    end
  rescue
    _ in [ArgumentError, FunctionClauseError, MatchError, KeyError] -> {:error, Vaisto.Errors.interface_invalid_format()}
  catch
    {:type_error, _} -> {:error, Vaisto.Errors.interface_invalid_format()}
  end
  def validate_core(_), do: {:error, Vaisto.Errors.interface_invalid_format()}

  defp validate_constraints(entries, exports, types, classes) do
    Enum.reduce_while(entries, {:ok, %{}}, fn
      [f | cs], {:ok, acc} when is_atom(f) and cs != [] ->
        export = Enum.find(exports, fn [name, _, _] -> name == f end)
        {binders, arrow} = case export do
          [^f, [:forall, bs, a], :plain] -> {bs, a}
          [^f, a, :plain] -> {[], a}
          _ -> {[], nil}
        end
        valid = case arrow do
          [:->, dictionaries, [:eff, :closed], [:->, _, _, _]] ->
            length(cs) == length(dictionaries) and Enum.zip(cs, dictionaries) |> Enum.all?(fn
              {[c, t, d], expected} when is_atom(c) ->
                target = [:->, [], [:eff, :closed], t]
                target = if binders == [], do: target, else: [:forall, binders, target]
                Map.has_key?(classes, c) and d == expected and
                  d == Vaisto.Elab.Classes.dictionary_type(classes, c, t) and
                  valid_export?([f, target, :plain], types)
              _ -> false
            end)
          _ -> false
        end
        if valid and not Map.has_key?(acc, f) and Enum.uniq(cs) == cs do
          {:cont, {:ok, Map.put(acc, f, Enum.map(cs, fn [c, t, d] -> {c, t, d} end))}}
        else
          {:halt, :error}
        end
      _, _ -> {:halt, :error}
    end)
  end

  defp validate_classes(entries, exports, types) do
    alias Vaisto.Elab.{Classes, Types}
    builtins = Classes.builtin_schemas()
    result = Enum.reduce_while(entries, {:ok, %{}, builtins}, fn
      [c, a, methods], {:ok, public, schemas} when is_atom(c) and is_atom(a) and is_list(methods) and methods != [] ->
        valid = Enum.all?(methods, fn
          [m, [:->, _, _, _] = t, default] when is_atom(m) and is_atom(default) ->
            Types.variables(t) == [{a, :Type}] and valid_export?([m, Types.generalize(t), :plain], types)
          _ -> false
        end)
        names = for [m, _, _] <- methods, do: m
        if valid and Enum.uniq(names) == names and not Map.has_key?(schemas, c) do
          info = %{param: a, methods: Enum.map(methods, fn [m, t, d] -> {m, t, [], d} end)}
          schemas = Map.put(schemas, c, info)
          defaults_valid = Enum.all?(methods, fn
            [_, _, :none] -> true
            [_, [:->, ps, eff, ret], f] ->
              expected = Types.generalize([:->, [Classes.dictionary_type(schemas, c, [:tvar, a]) | ps], eff, ret])
              [f, expected, :plain] in exports
          end)
          if defaults_valid do
            public_info = %{param: a, methods: Enum.map(methods, fn [m, t, d] -> {m, t, d} end)}
            {:cont, {:ok, Map.put(public, c, public_info), schemas}}
          else
            {:halt, :error}
          end
        else
          {:halt, :error}
        end
      _, _ -> {:halt, :error}
    end)
    case result do
      {:ok, public, schemas} ->
        names = for {_, info} <- schemas, {m, _, _, _} <- info.methods, do: m
        if Enum.uniq(names) == names, do: {:ok, public, schemas}, else: :error
      _ -> :error
    end
  end

  defp validate_instances(entries, exports, classes) do
    alias Vaisto.Elab.{Classes, Types}
    Enum.reduce_while(entries, {:ok, []}, fn
      [c, type, constraints, f], {:ok, instances} when is_atom(c) and is_atom(f) and is_list(constraints) ->
        valid_constraints = Enum.all?(constraints, fn
          [class, t] -> Map.has_key?(classes, class) and Enum.all?(Types.tvars(t), &(&1 in Types.tvars(type)))
          _ -> false
        end)
        head_valid = case type do
          [name | vars] when is_atom(name) and name != :tvar ->
            Enum.all?(vars, &match?([:tvar, a] when is_atom(a), &1)) and Enum.uniq(vars) == vars
          name when is_atom(name) -> true
          _ -> false
        end
        if Map.has_key?(classes, c) and head_valid and valid_constraints and Enum.uniq(constraints) == constraints do
          params = Enum.map(constraints, fn [class, t] -> Classes.dictionary_type(classes, class, t) end)
          expected = Types.generalize([:->, params, [:eff, :closed], Classes.dictionary_type(classes, c, type)])
          overlap = Enum.any?(instances, &(&1.class == c and Classes.head(&1.type) == Classes.head(type)))
          if [f, expected, :plain] in exports and not overlap do
            instance = %{class: c, type: type, factory: f, constraints: Enum.map(constraints, fn [class, t] -> {class, t} end)}
            {:cont, {:ok, instances ++ [instance]}}
          else
            {:halt, :error}
          end
        else
          {:halt, :error}
        end
      _, _ -> {:halt, :error}
    end)
  end

  defp valid_export?([f, type, status], types) when is_atom(f) and status in [:plain, :refined] do
    arrow = case type do
      [:forall, _, arrow] -> arrow
      arrow -> arrow
    end
    case arrow do
      [:->, params, _, _] ->
        ps = Enum.with_index(params, fn t, i ->
          candidate = :"__interface_arg#{i}"
          [if(candidate == f, do: :"__interface_arg#{i}_", else: candidate), t]
        end)
        stub = [:def, f, type, [:fn, ps, [:perform, :crash, [:inj, [:Reason], :no_match]]]]
        Lint.check_module([:module, :Interface, [:"core-version", 0] | types ++ [stub]]) == :ok
      _ -> false
    end
  end
  defp valid_export?(_, _), do: false

  @doc "Write the new canonical .vsi format; this never writes an HM type."
  def save_core(tree, dir) do
    with {:ok, interface} <- validate_core(tree) do
      path = interface_path(interface.module, dir)
      with :ok <- File.mkdir_p(Path.dirname(path)), do: File.write(path, Canonical.encode(tree))
    end
  end

  @doc "Read only canonical interfaces. Unknown symbols fail closed unless explicitly trusted."
  def load_core(module, paths, opts \\ []) do
    case find_interface(module, paths) do
      {:ok, path} ->
        with {:ok, binary} <- File.read(path),
             {:ok, tree} <- Canonical.decode(binary, Keyword.take(opts, [:atoms])),
             {:ok, interface} <- validate_core(tree),
             true <- bare_module(interface.module) == bare_module(module) do
          {:ok, interface}
        else
          _ -> {:error, Vaisto.Errors.corrupt_interface(path)}
        end
      :not_found -> {:error, Vaisto.Errors.interface_not_found(module)}
    end
  end

  defp bare_module(m), do: m |> Atom.to_string() |> String.replace_prefix("Elixir.", "")

  @doc """
  Extract module declarations from parsed AST.

  Returns {module_name, imports} or {:error, reason}
  """
  @spec extract_declarations(term()) :: {atom() | nil, [term()]}
  def extract_declarations(ast) when is_list(ast) do
    ns = find_ns(ast)
    imports = find_imports(ast)
    {ns, imports}
  end

  def extract_declarations(_ast), do: {nil, []}

  # Private helpers

  defp interface_path(module_name, dir) do
    # Strip Elixir. prefix for file paths: Elixir.Vaisto.Lexer → Vaisto/Lexer.vsi
    filename = module_name
      |> to_string()
      |> String.replace_prefix("Elixir.", "")
      |> String.replace(".", "/")
    Path.join(dir, "#{filename}.vsi")
  end

  defp find_interface(module_name, search_paths) do
    # Strip Elixir. prefix for file paths: Elixir.Vaisto.Lexer → Vaisto/Lexer.vsi
    filename = module_name
      |> to_string()
      |> String.replace_prefix("Elixir.", "")
      |> String.replace(".", "/")

    Enum.find_value(search_paths, :not_found, fn dir ->
      path = Path.join(dir, "#{filename}.vsi")
      if File.exists?(path), do: {:ok, path}
    end)
  end

  defp find_ns(ast) do
    Enum.find_value(ast, fn
      {:ns, name, _loc} -> name
      _ -> nil
    end)
  end

  defp find_imports(ast) do
    ast
    |> Enum.filter(&match?({:import, _, _, _}, &1))
    |> Enum.map(fn {:import, module, alias_name, _loc} ->
      {module, alias_name}
    end)
  end
end
