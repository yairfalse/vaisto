defmodule Vaisto.Refine do
  @moduledoc """
  Refinement checking, Phase 2.1 (RFC §4.4, §21): `Int`, `Bool` and `List`
  refinements on the parameters and results of single-clause functions,
  checked with Z3.

  It runs through the bridge of RFC §21.1. HM checks the plain program, the
  Phase 0 adapter turns HM's output into Liquid Core, and the refined
  signatures are attached to that Core. The program then runs on today's
  backends, so the bridge is gated:

    1. every definition that has refinements, or uses a function that does,
       must be in the Core fragment, pass Core Lint, and use only constructs
       the backends agree on (no externs, no effects but `crash`, no `==` on
       Float);
    2. the square must commute: erasing the refined Core gives back the Core
       the backends' program came from (K11);
    3. any failure is a build error, never "runs unchecked".

  A program without refinements never reaches this module, and never starts
  the solver.
  """

  alias Vaisto.Error
  alias Vaisto.Liquid.{Adapter, Lint}
  alias Vaisto.Refine.{IR, Render, Solver, Surface, VC, VCGen}

  @doc """
  Check a program's refinements. `typed` is HM's output for the plain
  program and `sigs` the refined signatures from `Surface.split/1`.

  Options: `:solver` (default `Solver.Z3`), `:source` (for error spans),
  `:locs` (definition name to `%Loc{}`), and solver options such as `:rlimit`.
  """
  @spec check(term(), %{atom() => Surface.Sig.t()}, keyword()) :: :ok | {:error, [Error.t()]}
  def check(typed, sigs, opts) do
    with :ok <- imported_calls(typed, Keyword.get(opts, :env, %{}), opts) do
      if map_size(sigs) == 0, do: :ok, else: check_refined(typed, sigs, opts)
    end
  end

  # Interfaces do not carry refinements until Phase 1b (§8.1), so a call to an
  # imported refined function could not be checked. It is refused, and
  # refinements are checked within one module (§21.2, C12).
  defp imported_calls(typed, env, opts) do
    refined = Map.get(env, :__refined_imports__, MapSet.new())

    calls =
      if MapSet.size(refined) == 0, do: [], else: qualified_calls(typed) |> Enum.filter(&MapSet.member?(refined, &1)) |> Enum.uniq()

    case calls do
      [] ->
        :ok

      calls ->
        {:error,
         for call <- calls do
           [mod, f] = call |> Atom.to_string() |> String.split(":", parts: 2)
           Error.new("`#{mod}/#{f}` has refinements, and calls across modules are not checked yet",
             hint: "call it from inside `#{mod}`; checking imported refinements needs refined interfaces (RFC C12)",
             span: Keyword.get(opts, :span)
           )
         end}
    end
  end

  defp qualified_calls({:qualified, mod, f}), do: [:"#{mod}:#{f}"]
  defp qualified_calls(term) when is_tuple(term), do: term |> Tuple.to_list() |> qualified_calls()
  defp qualified_calls(term) when is_list(term), do: Enum.flat_map(term, &qualified_calls/1)
  defp qualified_calls(_term), do: []

  defp check_refined(typed, sigs, opts) do
    {:ok, core, skipped} = Adapter.module(typed)
    refined = MapSet.new(Map.keys(sigs))

    with :ok <- gate_forms(typed, core, skipped, refined, opts),
         involved = involved(core, refined),
         :ok <- gate_lint(core, opts),
         :ok <- commutes(core, sigs, opts),
         {:ok, lsigs} <- logic_sigs(sigs, core, opts),
         {vcs, []} <- VCGen.generate(core, lsigs, involved) do
      solve(vcs, opts)
    else
      {:error, _} = error -> error
      {_vcs, gate} -> {:error, Enum.map(Enum.uniq(gate), &gate_error(&1, opts))}
    end
  end

  # --- Gates (§21.1) -----------------------------------------------------------------

  # A form the adapter did not translate is not checked, so it must not use a
  # refined function: its calls would otherwise run unchecked.
  defp gate_forms(typed, core, skipped, refined, opts) do
    adapted = for [:def, f | _] <- core, into: MapSet.new(), do: f
    skipped = Map.new(skipped)

    unchecked =
      typed
      |> forms()
      |> Enum.reject(&declaration?/1)
      |> Enum.map(&{definition_name(&1), &1})
      |> Enum.filter(fn {name, form} ->
        (name == nil or not MapSet.member?(adapted, name)) and (mentions?(form, refined) or MapSet.member?(refined, name))
      end)

    problems =
      for {name, _form} <- unchecked do
        case Map.fetch(skipped, name) do
          {:ok, why} -> {name, "refinement checking cannot read `#{name}` yet (#{why}), and it has or uses refinements"}
          :error -> {name, "this form uses a refined function, but only calls inside a `defn` are checked"}
        end
      end

    if problems == [], do: :ok, else: {:error, Enum.map(problems, &gate_error(&1, opts))}
  end

  # A1 (§4.4): Core Lint's rules hold for the program. Every definition, not
  # only the refined ones and their callers: a value HM mistyped elsewhere
  # (a Float typed Int, D17) reaches refined code through ordinary calls.
  defp gate_lint(core, opts) do
    case Lint.check_module(core) do
      :ok ->
        :ok

      {:error, problems} ->
        problems = for %{in: f, message: m} <- problems, uniq: true, do: {f, m}
        {:error, Enum.map(problems, fn {f, m} -> gate_error({f, "refinement checking needs `#{f}` to be well typed: #{m}"}, opts) end)}
    end
  end

  # K11: erase(elaborate_refined(P)) = adapter(HM(P)). The refined elaboration
  # writes each refined position's type from the signature's own base type,
  # independently of HM, so erasure gives back the adapter's Core exactly when
  # every refinement is attached to the term HM typed (RFC §21.1).
  defp commutes(core, sigs, opts) do
    defs = for [:def, f, type, _] <- core, into: %{}, do: {f, type}

    problems =
      for {f, %Surface.Sig{} = sig} <- Enum.sort(sigs),
          type = Map.get(defs, f),
          type != nil,
          erase(elaborate(sig, type)) != type do
        {f, "the refined signature of `#{f}` does not match its definition; this is a compiler bug (the refinement bridge does not commute)"}
      end

    if problems == [], do: :ok, else: {:error, Enum.map(problems, &gate_error(&1, opts))}
  end

  defp elaborate(sig, [:forall, binders, type]), do: [:forall, binders, elaborate(sig, type)]

  defp elaborate(%Surface.Sig{params: params, result: {rbase, rref}}, [:->, ptypes, eff, rtype]) when length(params) == length(ptypes) do
    named = for {{x, base, ref}, t} <- Enum.zip(params, ptypes), do: [x, refined_type(base, t, ref)]
    [:pi, named, eff, refined_type(rbase, rtype, rref)]
  end

  defp elaborate(_sig, type), do: [:mismatch, type]

  # An unrefined position keeps the adapter's type; a refined one is written
  # from the surface base type.
  defp refined_type(_base, type, nil), do: type
  defp refined_type(base, _type, {binder, pred}), do: [:refine, binder, core_type(base), {:surface, pred}]

  defp core_type(:int), do: :Int
  defp core_type(:bool), do: [:Bool]
  defp core_type(:float), do: :Float
  defp core_type(:string), do: :String
  defp core_type({:call, :List, [elem], _loc}), do: [:List, core_type(elem)]
  defp core_type(other), do: [:unknown, other]

  defp erase([:forall, binders, type]), do: [:forall, binders, erase(type)]
  defp erase([:pi, named, eff, rtype]), do: [:->, Enum.map(named, fn [_x, t] -> erase(t) end), eff, erase(rtype)]
  defp erase([:refine, _binder, type, _pred]), do: type
  defp erase(type), do: type

  defp logic_sigs(sigs, core, opts) do
    types = for [:def, f, type, _] <- core, into: %{}, do: {f, type}

    case VCGen.logic_sigs(sigs, types) do
      {:ok, lsigs} -> {:ok, lsigs}
      {:error, problems} -> {:error, Enum.map(problems, &gate_error(&1, opts))}
    end
  end

  # Refined definitions and every definition that uses one.
  defp involved(core, refined) do
    users = for [:def, f, _type, fun] <- core, mentions?(fun, refined), into: MapSet.new(), do: f
    MapSet.union(refined, users)
  end

  defp forms({:module, forms}), do: forms
  defp forms(form), do: [form]

  # Forms without code. Classes and instances have method bodies, so they are
  # not declarations here: the adapter does not translate them, and they must
  # not use refined functions.
  defp declaration?(form) when is_tuple(form), do: elem(form, 0) in [:deftype, :extern, :ns, :import, :defprompt]
  defp declaration?(_form), do: false

  # The name a top-level form defines, or nil for an expression. Only these
  # forms define anything; elem(form, 1) of a call is its callee.
  defp definition_name(form) when is_tuple(form) and elem(form, 0) in [:defn, :defn_multi, :defval, :process], do: elem(form, 1)
  defp definition_name(_form), do: nil

  # Whether a term mentions any of the names, anywhere. Conservative: a local
  # binder spelled like a refined function counts too.
  defp mentions?(term, names) when is_atom(term), do: MapSet.member?(names, term)
  defp mentions?(term, names) when is_list(term), do: Enum.any?(term, &mentions?(&1, names))
  defp mentions?(term, names) when is_tuple(term), do: term |> Tuple.to_list() |> mentions?(names)
  defp mentions?(term, names) when is_map(term), do: term |> Map.to_list() |> mentions?(names)
  defp mentions?(_term, _names), do: false

  # --- Solving and diagnostics -------------------------------------------------------

  defp solve(vcs, opts) do
    solver = Keyword.get(opts, :solver, Solver.Z3)

    # §11.3: an obligation proved only because the facts at it contradict
    # each other proves nothing. Ask about the facts alone.
    contradictions =
      for %VC{kind: kind} = vc <- vcs, kind in [:precondition, :postcondition, :primitive_safety], vc.hyps != [] do
        %VC{id: {:contradiction, vc.id}, kind: :contradiction, hyps: vc.hyps, goal: {:bool, false}, vars: IR.vars(vc.hyps), meta: %{of: vc.id}}
      end

    all = vcs ++ contradictions

    if solver == Solver.Z3 and System.find_executable(Keyword.get(opts, :z3, "z3")) == nil do
      {:error, [Error.new("refinement checking needs the `z3` solver, which was not found on PATH", hint: "install z3, for example `brew install z3` or `apt install z3`")]}
    else
      answers = all |> solver.check(opts) |> Map.new()
      errors = diagnose(vcs, answers, opts)
      if errors == [], do: :ok, else: {:error, errors}
    end
  end

  defp diagnose(vcs, answers, opts) do
    # A definition whose requirements can never be met, or whose result can
    # never hold, is reported once; its other findings would only repeat it.
    vacuous = fn about ->
      for %VC{kind: :vacuity, meta: %{def: f, about: ^about}} = vc <- vcs,
          not match?({:invalid, _}, Map.get(answers, vc.id)),
          into: MapSet.new(),
          do: f
    end

    unmeetable = vacuous.(:requirements)
    silenced = MapSet.union(unmeetable, vacuous.(:guarantee))

    vcs
    |> Enum.flat_map(fn vc ->
      answer = Map.get(answers, vc.id, {:unknown, :no_answer})

      cond do
        vc.kind == :vacuity and vc.meta.about == :guarantee and MapSet.member?(unmeetable, vc.meta.def) -> []
        vc.kind == :vacuity -> vacuity_error(vc, answer, opts)
        MapSet.member?(silenced, vc.meta.def) -> []
        true -> obligation_error(vc, answer, Map.get(answers, {:contradiction, vc.id}), opts)
      end
    end)
    |> Enum.uniq_by(&{&1.message, &1.primary_span, &1.note})
    |> Enum.sort_by(fn e -> {(e.primary_span || %{line: 0})[:line], (e.primary_span || %{col: 0})[:col]} end)
  end

  defp vacuity_error(%VC{meta: %{about: :requirements, def: f}}, answer, opts) do
    case answer do
      {:invalid, _} -> []
      :valid -> [Error.new("the requirements of `#{f}` can never be met", span: def_span(f, opts, "no argument satisfies them all"))]
      {:unknown, why} -> [Error.new("could not decide whether the requirements of `#{f}` can be met", span: def_span(f, opts, unknown_label(why)))]
    end
  end

  defp vacuity_error(%VC{meta: %{about: :guarantee, def: f}}, answer, opts) do
    case answer do
      {:invalid, _} -> []
      :valid -> [Error.new("the declared result of `#{f}` can never hold", span: def_span(f, opts, "no value satisfies it, so `#{f}` can only verify by never returning"))]
      {:unknown, why} -> [Error.new("could not decide whether the declared result of `#{f}` can hold", span: def_span(f, opts, unknown_label(why)))]
    end
  end

  defp obligation_error(vc, answer, contradiction, opts) do
    case {answer, contradiction} do
      {:valid, nil} -> []
      {:valid, {:invalid, _}} -> []
      {:valid, :valid} -> [contradiction_error(vc, opts)]
      {:valid, {:unknown, why}} -> [unknown_error(vc, why, opts)]
      {{:invalid, _}, _} -> [failure_error(vc, opts)]
      {{:unknown, why}, _} -> [unknown_error(vc, why, opts)]
    end
  end

  defp failure_error(%VC{kind: :postcondition, meta: m}, opts) do
    requirement = Render.surface(m.conjunct, %{})
    label = "this result, #{Render.core(m.expr)}, must satisfy #{requirement}"
    span = expr_span(m.def, m.expr, opts, label, Map.get(m, :within))
    Error.new("result does not satisfy the declared type", span: span, note: facts_note(m))
  end

  defp failure_error(%VC{kind: :primitive_safety, meta: m}, opts) do
    Error.new("requirement not met", span: expr_span(m.def, m.expr, opts, m.label), note: facts_note(m))
  end

  defp failure_error(%VC{kind: :precondition, meta: %{about: :as_value} = m}, opts) do
    requirement = Render.surface(m.conjunct, m.subst)
    label = "`#{m.callee}` is used as a value here, so #{requirement} must hold for every argument `#{m.param}`"
    Error.new("requirement not met", span: expr_span(m.def, m.expr, opts, label), hint: "call it directly, where its argument can be checked")
  end

  defp failure_error(%VC{kind: :precondition, meta: m}, opts) do
    requirement = Render.surface(m.conjunct, m.subst)
    label = "`#{m.callee}` requires #{requirement} for its argument `#{m.param}`"
    Error.new("requirement not met", span: arg_span(m, opts, label), note: facts_note(m))
  end

  defp contradiction_error(%VC{meta: m}, opts) do
    Error.new("this code can only run under contradictory facts",
      span: expr_span(m.def, m.expr, opts, "its requirement holds only because nothing can reach it"),
      note: facts_note(m),
      hint: "the facts on this path contradict each other; check the conditions that lead here"
    )
  end

  defp unknown_error(%VC{meta: m}, why, opts) do
    label =
      case m do
        %{about: :precondition} -> "`#{m.callee}` requires #{Render.surface(m.conjunct, m.subst)}"
        %{about: :result} -> "this result must satisfy #{Render.surface(m.conjunct, %{})}"
        %{label: label} -> label
        _ -> "this requirement"
      end

    Error.new("could not verify this requirement within its resource limit", span: expr_span(m.def, m.expr, opts, label), note: unknown_label(why))
  end

  defp unknown_label(:solver_missing), do: "the `z3` solver was not found on PATH"
  defp unknown_label(:timeout), do: "the solver did not answer in time"
  defp unknown_label(_why), do: "the solver could not decide it"

  defp facts_note(%{notes: []} = m) do
    case Map.get(m, :arg) do
      x when is_atom(x) and x != nil -> "nothing is known about `#{Render.source_name(x)}` here"
      _ -> nil
    end
  end

  defp facts_note(%{notes: notes}) do
    facts = Enum.map_join(notes, ", and ", fn {text, true} -> "#{text} holds"; {text, false} -> "#{text} does not hold" end)
    "in this branch, #{facts}"
  end

  defp gate_error({f, message}, opts), do: Error.new(message, span: def_span(f, opts, nil))

  # --- Spans -------------------------------------------------------------------------
  # Core carries no source spans yet (RFC §9.4), so a diagnostic is placed by
  # finding the expression's text in its definition. When the text is not
  # found, the span is the definition's.

  defp def_span(f, opts, label) do
    case Map.get(Keyword.get(opts, :locs, %{}), f) do
      %{line: line, col: col} -> %{line: line, col: col, length: 1, label: label}
      _ -> nil
    end
  end

  defp expr_span(f, expr, opts, label, within \\ nil) do
    text = Render.core(expr)

    case {locate(f, text, opts), within} do
      {{line, col}, _} ->
        %{line: line, col: col, length: String.length(text), label: label}

      # A bare name or literal is not distinctive, so place it by its position
      # in the `if` it is a branch of.
      {nil, {parent, branch}} ->
        with {line, col} <- locate(f, Render.core(parent), opts),
             offset when is_integer(offset) <- branch_offset(parent, branch) do
          %{line: line, col: col + offset, length: String.length(text), label: label}
        else
          _ -> def_span(f, opts, label)
        end

      {nil, nil} ->
        def_span(f, opts, label)
    end
  end

  # Where branch 2 (then) or 3 (else) of an `if` starts in its rendering.
  defp branch_offset([:if, c, _a, [:inj, [:Bool], false]], 2), do: String.length("(and " <> Render.core(c) <> " ")
  defp branch_offset([:if, c, [:inj, [:Bool], true], _b], 3), do: String.length("(or " <> Render.core(c) <> " ")
  defp branch_offset([:if, c, _a, _b], 2), do: String.length("(if " <> Render.core(c) <> " ")
  defp branch_offset([:if, c, a, _b], 3), do: String.length("(if " <> Render.core(c) <> " " <> Render.core(a) <> " ")
  defp branch_offset(_parent, _branch), do: nil

  # The caret goes under the argument, inside the call (§13.3).
  defp arg_span(m, opts, label) do
    call = Render.core(m.expr)
    arg = Render.core(m.arg)

    case locate(m.def, call, opts) do
      {line, col} ->
        offset = find_arg(call, arg)
        %{line: line, col: col + offset, length: String.length(arg), label: label}

      nil ->
        def_span(m.def, opts, label)
    end
  end

  defp find_arg(call, arg) do
    # Skip the callee's name, then find the argument as a whole token.
    [head | _] = String.split(call, " ", parts: 2)
    rest = binary_part(call, byte_size(head), byte_size(call) - byte_size(head))

    case Regex.run(~r/(?<![\w?!-])#{Regex.escape(arg)}(?![\w?!-])/u, rest, return: :index) do
      [{at, _}] -> String.length(head) + String.length(binary_part(rest, 0, at))
      _ -> 0
    end
  end

  # Only an expression with parentheses is distinctive enough to find by text.
  defp locate(f, "(" <> _ = text, opts) do
    with source when is_binary(source) <- Keyword.get(opts, :source),
         %{line: start} <- Map.get(Keyword.get(opts, :locs, %{}), f) do
      source
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.drop(start - 1)
      |> Enum.find_value(fn {line, n} ->
        case :binary.match(line, text) do
          {at, _} -> {n, String.length(binary_part(line, 0, at)) + 1}
          :nomatch -> nil
        end
      end)
    else
      _ -> nil
    end
  end

  defp locate(_f, _text, _opts), do: nil
end
