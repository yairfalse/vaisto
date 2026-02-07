defmodule Vaisto.CoreEmitter do
  @moduledoc """
  Emits Core Erlang AST from typed Vaisto AST and compiles to BEAM.

  This is the "deep" path - direct compilation to BEAM bytecode
  without going through Elixir. More control, tighter output.

  Core Erlang is built using the :cerl module which constructs
  the AST nodes that :compile.forms/2 understands.
  """

  alias Vaisto.Backend.Shared
  alias Vaisto.Error
  alias Vaisto.TypeChecker

  @doc """
  Compile typed AST directly to BEAM bytecode.

  Options:
    - :load - whether to load the module into the VM (default: true)

  Returns `{:ok, module_name, binary}` or `{:error, %Vaisto.Error{}}`.
  """
  @spec compile(TypeChecker.typed_ast(), atom(), keyword()) ::
          {:ok, atom(), binary()} | {:error, Error.t()}
  def compile(typed_ast, module_name \\ :VaistoModule, opts \\ [])

  def compile(typed_ast, module_name, opts) when is_list(opts) do
    load? = Keyword.get(opts, :load, true)

    try do
      core_ast = to_core(typed_ast, module_name)

      case :compile.forms(core_ast, [:from_core, :binary, :return_errors]) do
        {:ok, ^module_name, binary} ->
          if load?, do: :code.load_binary(module_name, ~c"vaisto", binary)
          {:ok, module_name, binary}

        {:ok, ^module_name, binary, _warnings} ->
          if load?, do: :code.load_binary(module_name, ~c"vaisto", binary)
          {:ok, module_name, binary}

        {:error, errors, _warnings} ->
          formatted = try do
            format_errors(errors)
          rescue
            _ -> "Internal compiler error: #{inspect(errors, limit: :infinity)}"
          end
          {:error, Error.new("BEAM compilation failed", note: formatted)}
      end
    rescue
      e -> {:error, Error.new("compilation error", note: Exception.message(e))}
    end
  end

  @doc """
  Compile and load the module into the VM.
  """
  @spec compile_and_load(TypeChecker.typed_ast(), atom()) ::
          {:ok, atom()} | {:error, Error.t()}
  def compile_and_load(typed_ast, module_name \\ :VaistoModule) do
    case compile(typed_ast, module_name) do
      {:ok, mod, binary} ->
        :code.load_binary(mod, ~c"vaisto", binary)
        {:ok, mod}

      error ->
        error
    end
  end

  @doc """
  Transform typed Vaisto AST to Core Erlang AST.
  """
  @spec to_core(TypeChecker.typed_ast(), atom()) :: :cerl.cerl()
  def to_core(typed_ast, module_name)

  # Process definition → raw BEAM process module
  def to_core({:process, _name, _init, handlers, _type}, module_name) do
    to_core_process(module_name, handlers)
  end

  # Module with function definitions or process
  def to_core({:module, forms}, module_name) do
    # Check if this module defines a process
    process_def = Enum.find(forms, fn
      {:process, _, _, _, _} -> true
      _ -> false
    end)

    if process_def do
      {:process, _name, _init, handlers, _type} = process_def
      to_core_process(module_name, handlers)
    else
      to_core_module_forms(forms, module_name)
    end
  end

  # Single defn → wrap in module and compile
  def to_core({:defn, _, _, _, _} = defn, module_name) do
    to_core({:module, [defn]}, module_name)
  end

  # Single defval → wrap in module and compile
  def to_core({:defval, _, _, _} = defval, module_name) do
    to_core({:module, [defval]}, module_name)
  end

  # Single deftype → wrap in module (generates constructor functions)
  def to_core({:deftype, _, _, _} = deftype, module_name) do
    to_core({:module, [deftype]}, module_name)
  end

  # Single expression → module with main/0
  def to_core(typed_ast, module_name) do
    main_body = to_core_expr(typed_ast)
    main_fun = :cerl.c_fun([], main_body)
    main_name = :cerl.c_fname(:main, 0)

    :cerl.c_module(
      :cerl.c_atom(module_name),
      [main_name],
      [],
      [{main_name, main_fun}]
    )
  end

  defp to_core_module_forms(forms, module_name) do
    # Separate defn forms from other expressions
    {defns, exprs} = Enum.split_with(forms, fn
      {:defn, _, _, _, _} -> true
      {:defn_multi, _, _, _, _} -> true  # 5 elements: name, arity, clauses, type
      {:defval, _, _, _} -> true   # Value bindings
      {:process, _, _, _, _} -> true # Process definition
      {:deftype, _, _, _} -> true  # Keep deftypes for constructor generation
      {:extern, _, _, _} -> true   # Skip externs (compile-time only)
      {:ns, _} -> true             # Skip ns (compile-time only)
      {:import, _, _} -> true      # Skip import (compile-time only)
      _ -> false
    end)

    # Extract sum types to generate constructor functions
    sum_types = Enum.filter(defns, fn
      {:deftype, _name, {:sum, _variants}, _type} -> true
      _ -> false
    end)

    # Generate constructor functions for each sum type variant
    # E.g., (deftype Token (LParen loc)) → function LParen/1 that returns {:LParen, loc}
    constructor_fns = Enum.flat_map(sum_types, fn {:deftype, _name, {:sum, variants}, _type} ->
      Enum.map(variants, fn {ctor_name, arg_types} ->
        arity = length(arg_types)
        # Create parameter variables: __arg0__, __arg1__, etc.
        param_vars = Enum.map(0..(arity - 1)//1, fn i -> :cerl.c_var(:"__arg#{i}__") end)
        # Body: tuple of constructor name and args: {:Ctor, arg0, arg1, ...}
        body = :cerl.c_tuple([:cerl.c_atom(ctor_name) | param_vars])
        fun = :cerl.c_fun(param_vars, body)
        fname = :cerl.c_fname(ctor_name, arity)
        {fname, fun}
      end)
    end)

    # Filter out deftypes, keep only defns, defn_multi, defval, and process
    # Note: defn_multi from type checker has 5 elements: {:defn_multi, name, arity, clauses, type}
    defns = Enum.filter(defns, fn
      {:defn, _, _, _, _} -> true
      {:defn_multi, _, _, _, _} -> true
      {:defval, _, _, _} -> true
      {:process, _, _, _, _} -> true
      _ -> false
    end)

    # Track user-defined function names for local calls
    # defn_multi now has arity as 3rd element from type checker
    # defval becomes a zero-arity function
    user_fns = defns
      |> Enum.map(fn
        {:defn, name, params, _, _} -> {name, length(params)}
        {:defn_multi, name, arity, _clauses, _type} -> {name, arity}
        {:defval, name, _, _} -> {name, 0}
      end)
      |> MapSet.new()

    # Build function definitions
    fun_defs = Enum.map(defns, fn
      {:defn, name, params, body, _type} ->
        param_vars = Enum.map(params, &:cerl.c_var/1)
        # Track parameters as local variables so they can be called as functions
        local_vars = MapSet.new(params)
        body_core = to_core_expr(body, user_fns, local_vars)
        fun = :cerl.c_fun(param_vars, body_core)
        fname = :cerl.c_fname(name, length(params))
        {fname, fun}

      {:defn_multi, name, arity, clauses, _type} ->
        # Multi-clause function: single function with case over argument
        # Create a single argument variable to match on
        arg_var = :cerl.c_var(:__arg__)
        param_vars = [arg_var]

        # Build case clauses from patterns
        # Note: pattern variables are bound within each clause
        case_clauses = Enum.map(clauses, fn {pattern, body, _body_type} ->
          pattern_core = to_core_multi_pattern(pattern)
          pattern_vars = Shared.extract_pattern_vars(pattern)
          local_vars = MapSet.new(pattern_vars)
          body_core = to_core_expr(body, user_fns, local_vars)
          :cerl.c_clause([pattern_core], :cerl.c_atom(true), body_core)
        end)

        fun_body = :cerl.c_case(arg_var, case_clauses)
        fun = :cerl.c_fun(param_vars, fun_body)
        fname = :cerl.c_fname(name, arity)
        {fname, fun}

      # Value binding becomes a zero-arity function that returns the value
      {:defval, name, value, _type} ->
        body_core = to_core_expr(value, user_fns)
        fun = :cerl.c_fun([], body_core)
        fname = :cerl.c_fname(name, 0)
        {fname, fun}
    end)

    # Build main/0 from remaining expressions (if any)
    {main_exports, main_defs} = case exprs do
      [] ->
        {[], []}
      _ ->
        # Combine expressions, use the last one as the result
        main_body = case exprs do
          [single] -> to_core_expr(single, user_fns)
          multiple ->
            # Sequence all expressions, return last
            [last | rest] = Enum.reverse(multiple)
            Enum.reduce(rest, to_core_expr(last, user_fns), fn expr, acc ->
              :cerl.c_seq(to_core_expr(expr, user_fns), acc)
            end)
        end
        main_fun = :cerl.c_fun([], main_body)
        main_name = :cerl.c_fname(:main, 0)
        {[main_name], [{main_name, main_fun}]}
    end

    # Export all user-defined functions plus main plus constructor functions
    fun_exports = Enum.map(fun_defs, fn {fname, _} -> fname end)
    ctor_exports = Enum.map(constructor_fns, fn {fname, _} -> fname end)

    :cerl.c_module(
      :cerl.c_atom(module_name),
      fun_exports ++ main_exports ++ ctor_exports,
      [],
      fun_defs ++ main_defs ++ constructor_fns
    )
  end

  # --- Process compilation ---
  # Generates a raw BEAM process with start_link/1 and loop/1

  defp to_core_process(module_name, handlers) do
    state_var = :cerl.c_var(:state)
    from_var = :cerl.c_var(:from)
    init_var = :cerl.c_var(:init)

    # Build receive clauses from handlers
    clauses = Enum.map(handlers, fn {msg, body, _type} ->
      to_core_handler_clause(module_name, msg, body, state_var, from_var)
    end)

    # loop/1: receive handlers, recurse
    recv_expr = :cerl.c_receive(clauses, :cerl.c_atom(:infinity), :cerl.c_atom(:timeout))
    loop_fun = :cerl.c_fun([state_var], recv_expr)
    loop_name = :cerl.c_fname(:loop, 1)

    # start_link/1: spawn(Module, loop, [Init])
    spawn_call = :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:spawn),
      [
        :cerl.c_atom(module_name),
        :cerl.c_atom(:loop),
        :cerl.c_cons(init_var, :cerl.c_nil())
      ]
    )
    start_fun = :cerl.c_fun([init_var], spawn_call)
    start_name = :cerl.c_fname(:start_link, 1)

    :cerl.c_module(
      :cerl.c_atom(module_name),
      [start_name, loop_name],
      [],
      [{start_name, start_fun}, {loop_name, loop_fun}]
    )
  end

  defp to_core_handler_clause(module_name, msg, body, state_var, from_var) do
    # Unwrap {:atom, value} if present
    msg_atom = case msg do
      {:atom, a} -> a
      a when is_atom(a) -> a
    end
    # Pattern: {msg, From}
    pattern = :cerl.c_tuple([:cerl.c_atom(msg_atom), from_var])

    # Body expression (may reference state)
    body_expr = to_core_expr_with_state(body, state_var)

    # New state is the result of the body
    new_state_var = :cerl.c_var(:new_state)

    # let NewState = body in From ! NewState, loop(NewState)
    let_body = :cerl.c_seq(
      :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:!), [from_var, new_state_var]),
      :cerl.c_call(:cerl.c_atom(module_name), :cerl.c_atom(:loop), [new_state_var])
    )

    full_body = :cerl.c_let([new_state_var], body_expr, let_body)

    :cerl.c_clause([pattern], :cerl.c_atom(true), full_body)
  end

  # Expression transformation that handles state variable
  defp to_core_expr_with_state({:var, :state, _type}, state_var), do: state_var
  defp to_core_expr_with_state({:call, op, [left, right], _type}, state_var) when op in [:+, :-, :*, :/] do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(op),
      [to_core_expr_with_state(left, state_var), to_core_expr_with_state(right, state_var)]
    )
  end
  defp to_core_expr_with_state(other, _state_var), do: to_core_expr(other)

  # --- Expression transformation ---
  # Default: no user functions context, no local variables
  defp to_core_expr(ast), do: to_core_expr(ast, MapSet.new(), MapSet.new())
  defp to_core_expr(ast, user_fns), do: to_core_expr(ast, user_fns, MapSet.new())

  # Literals
  defp to_core_expr({:lit, :int, n}, _user_fns, _local_vars), do: :cerl.c_int(n)
  defp to_core_expr({:lit, :float, f}, _user_fns, _local_vars), do: :cerl.c_float(f)
  defp to_core_expr({:lit, :atom, a}, _user_fns, _local_vars), do: :cerl.c_atom(a)
  defp to_core_expr({:lit, :bool, b}, _user_fns, _local_vars), do: :cerl.c_atom(b)
  defp to_core_expr({:lit, :string, s}, _user_fns, _local_vars), do: :cerl.c_binary(string_to_binary_segments(s))
  defp to_core_expr({:lit, :unit, _}, _user_fns, _local_vars), do: :cerl.c_atom(nil)

  # List literal
  defp to_core_expr({:list, elements, _type}, user_fns, local_vars) do
    elements
    |> Enum.map(&to_core_expr(&1, user_fns, local_vars))
    |> Enum.reverse()
    |> Enum.reduce(:cerl.c_nil(), fn elem, acc -> :cerl.c_cons(elem, acc) end)
  end

  # Cons expression: [head | tail] → prepend head to tail list
  defp to_core_expr({:cons, head, tail, _type}, user_fns, local_vars) do
    :cerl.c_cons(
      to_core_expr(head, user_fns, local_vars),
      to_core_expr(tail, user_fns, local_vars)
    )
  end

  # Variables
  # If the variable is a zero-arity user function (defval), call it
  # Explicit function reference - module-level function passed as value
  # Creates a wrapper function: fun(X1, ...) -> name(X1, ...) end
  defp to_core_expr({:fn_ref, name, arity, _type}, user_fns, _local_vars) do
    if MapSet.member?(user_fns, {name, arity}) do
      # Create wrapper function: fun(X1, X2, ...) -> name(X1, X2, ...) end
      arg_vars = for i <- 1..arity, do: :cerl.c_var(:"__arg#{i}__")
      fun_body = :cerl.c_apply(:cerl.c_fname(name, arity), arg_vars)
      :cerl.c_fun(arg_vars, fun_body)
    else
      # External function - just reference by name
      :cerl.c_fname(name, arity)
    end
  end

  # If the variable has a function type and is a user function, create a fun reference
  # Otherwise, it's a regular variable reference
  defp to_core_expr({:var, name, type}, user_fns, local_vars) do
    cond do
      # Local variable takes precedence (let binding, function parameter)
      MapSet.member?(local_vars, name) ->
        :cerl.c_var(name)

      # Zero-arity user function (defval) - call it
      MapSet.member?(user_fns, {name, 0}) ->
        :cerl.c_apply(:cerl.c_fname(name, 0), [])

      # Function reference: variable with function type that's a user function
      # Create a fun wrapper: fun(Args) -> name(Args) end
      match?({:fn, _, _}, type) ->
        {:fn, arg_types, _ret} = type
        arity = length(arg_types)
        if MapSet.member?(user_fns, {name, arity}) do
          # Create wrapper function: fun(X1, X2, ...) -> name(X1, X2, ...) end
          arg_vars = for i <- 1..arity, do: :cerl.c_var(:"__arg#{i}__")
          fun_body = :cerl.c_apply(:cerl.c_fname(name, arity), arg_vars)
          :cerl.c_fun(arg_vars, fun_body)
        else
          :cerl.c_var(name)
        end

      # Regular variable
      true ->
        :cerl.c_var(name)
    end
  end

  # If expression → Core Erlang case on boolean
  defp to_core_expr({:if, condition, then_branch, else_branch, _type}, user_fns, local_vars) do
    cond_core = to_core_expr(condition, user_fns, local_vars)
    then_core = to_core_expr(then_branch, user_fns, local_vars)
    else_core = to_core_expr(else_branch, user_fns, local_vars)

    # case Cond of true -> Then; false -> Else end
    true_clause = :cerl.c_clause([:cerl.c_atom(true)], :cerl.c_atom(true), then_core)
    false_clause = :cerl.c_clause([:cerl.c_atom(false)], :cerl.c_atom(true), else_core)

    :cerl.c_case(cond_core, [true_clause, false_clause])
  end

  # Match expression → Core Erlang case
  defp to_core_expr({:match, expr, clauses, _type}, user_fns, local_vars) do
    expr_core = to_core_expr(expr, user_fns, local_vars)
    clause_cores = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_core = to_core_pattern(pattern)
      # Track pattern variables in the clause body
      pattern_vars = Shared.extract_pattern_vars(pattern)
      clause_local_vars = MapSet.union(local_vars, MapSet.new(pattern_vars))
      body_core = to_core_expr(body, user_fns, clause_local_vars)
      :cerl.c_clause([pattern_core], :cerl.c_atom(true), body_core)
    end)
    :cerl.c_case(expr_core, clause_cores)
  end

  # Raw tuple expression: {:tuple, elements, type} → Core Erlang tuple
  defp to_core_expr({:tuple, elements, _type}, user_fns, local_vars) do
    element_cores = Enum.map(elements, &to_core_expr(&1, user_fns, local_vars))
    :cerl.c_tuple(element_cores)
  end

  # Map literal: {:map, pairs, type} → Core Erlang map
  defp to_core_expr({:map, pairs, _type}, user_fns, local_vars) do
    pair_cores = Enum.map(pairs, fn {key, val} ->
      key_core = to_core_expr(key, user_fns, local_vars)
      val_core = to_core_expr(val, user_fns, local_vars)
      :cerl.c_map_pair(key_core, val_core)
    end)
    :cerl.c_map(pair_cores)
  end

  # Receive expression → Core Erlang receive
  # (receive [pattern body] ...) → receive pattern -> body end
  defp to_core_expr({:receive, clauses, _type}, user_fns, local_vars) do
    clause_cores = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_core = to_core_pattern(pattern)
      pattern_vars = Shared.extract_pattern_vars(pattern)
      clause_local_vars = MapSet.union(local_vars, MapSet.new(pattern_vars))
      body_core = to_core_expr(body, user_fns, clause_local_vars)
      :cerl.c_clause([pattern_core], :cerl.c_atom(true), body_core)
    end)
    # c_receive(clauses, timeout_expr, timeout_body)
    # infinity timeout, timeout body never executes
    :cerl.c_receive(clause_cores, :cerl.c_atom(:infinity), :cerl.c_atom(:timeout))
  end

  # Supervise expression → supervisor spec
  # returns {{Strategy, 1, 5}, [Children]}
  defp to_core_expr({:supervise, {:atom, strategy}, children, _type}, user_fns, local_vars) do
    strategy_atom = :cerl.c_atom(strategy)
    flags = :cerl.c_tuple([strategy_atom, :cerl.c_int(1), :cerl.c_int(5)])
    
    child_specs = Enum.map(children, fn child ->
      to_core_expr(child, user_fns, local_vars)
    end)
    
    child_list = List.foldr(child_specs, :cerl.c_nil(), fn child, acc ->
      :cerl.c_cons(child, acc)
    end)
    
    :cerl.c_tuple([:cerl.c_atom(:ok), :cerl.c_tuple([flags, child_list])])
  end

  # Let bindings: nest each binding as Core Erlang let
  # (let [x 1 y 2] body) → let x = 1 in let y = 2 in body
  # local_vars tracks variables in scope so they can be called as functions
  defp to_core_expr({:let, bindings, body, _type}, user_fns, local_vars) do
    # Collect all bound variable names from bindings
    bound_vars = Enum.flat_map(bindings, fn
      {name, _expr, _type} when is_atom(name) -> [name]
      {{:tuple_pattern, _, _} = pattern, _expr, _type} -> Shared.extract_pattern_vars(pattern)
      _ -> []
    end)

    # Body sees all let-bound variables plus existing local vars
    body_local_vars = MapSet.union(local_vars, MapSet.new(bound_vars))
    body_expr = to_core_expr(body, user_fns, body_local_vars)

    # Build from innermost to outermost
    List.foldr(bindings, body_expr, fn binding, acc ->
      emit_let_binding(binding, acc, user_fns, local_vars)
    end)
  end



  # Do block: sequence of expressions, return value of last
  defp to_core_expr({:do, [], _type}, _user_fns, _local_vars) do
    :cerl.c_atom(:ok)  # empty do returns :ok
  end

  defp to_core_expr({:do, [single], _type}, user_fns, local_vars) do
    to_core_expr(single, user_fns, local_vars)
  end

  defp to_core_expr({:do, [first | rest], type}, user_fns, local_vars) do
    # Use let with _ to sequence expressions
    first_expr = to_core_expr(first, user_fns, local_vars)
    rest_expr = to_core_expr({:do, rest, type}, user_fns, local_vars)
    :cerl.c_seq(first_expr, rest_expr)
  end

  # Arithmetic: (+ a b) → erlang:'+'(a, b)
  defp to_core_expr({:call, op, [left, right], _type}, user_fns, local_vars) when op in [:+, :-, :*, :/] do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(op),
      [to_core_expr(left, user_fns, local_vars), to_core_expr(right, user_fns, local_vars)]
    )
  end

  # Comparison operators
  defp to_core_expr({:call, op, [left, right], _type}, user_fns, local_vars) when op in [:==, :!=, :<, :>, :<=, :>=] do
    erlang_op = case op do
      :== -> :"=:="
      :!= -> :"/="
      :<= -> :"=<"   # Erlang uses =< for less-than-or-equal
      _ -> op        # <, >, >= are same in Erlang
    end

    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(erlang_op),
      [to_core_expr(left, user_fns, local_vars), to_core_expr(right, user_fns, local_vars)]
    )
  end

  # --- List operations ---

  # head: erlang:hd/1
  defp to_core_expr({:call, :head, [list_expr], _type}, user_fns, local_vars) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:hd),
      [to_core_expr(list_expr, user_fns, local_vars)]
    )
  end

  # tail: erlang:tl/1
  defp to_core_expr({:call, :tail, [list_expr], _type}, user_fns, local_vars) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:tl),
      [to_core_expr(list_expr, user_fns, local_vars)]
    )
  end

  # cons: [elem | list]
  defp to_core_expr({:call, :cons, [elem_expr, list_expr], _type}, user_fns, local_vars) do
    :cerl.c_cons(
      to_core_expr(elem_expr, user_fns, local_vars),
      to_core_expr(list_expr, user_fns, local_vars)
    )
  end

  # empty?: list == []
  defp to_core_expr({:call, :empty?, [list_expr], _type}, user_fns, local_vars) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:"=:="),
      [to_core_expr(list_expr, user_fns, local_vars), :cerl.c_nil()]
    )
  end

  # length: erlang:length/1
  defp to_core_expr({:call, :length, [list_expr], _type}, user_fns, local_vars) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:length),
      [to_core_expr(list_expr, user_fns, local_vars)]
    )
  end

  # str: string concatenation/conversion
  # Converts each arg to string and concatenates using erlang:iolist_to_binary/1
  defp to_core_expr({:call, :str, args, _type}, user_fns, local_vars) do
    # Convert each arg - strings pass through, others get converted
    # Build: erlang:iolist_to_binary([convert(a1), convert(a2), ...])
    # Where convert uses a case to handle strings vs other types
    converted_args = Enum.map(args, fn arg ->
      arg_core = to_core_expr(arg, user_fns, local_vars)
      arg_var = :cerl.c_var(:__str_arg__)

      # case Arg of
      #   X when is_binary(X) -> X;
      #   X when is_atom(X) -> atom_to_binary(X);
      #   X when is_integer(X) -> integer_to_binary(X);
      #   X when is_float(X) -> float_to_binary(X);
      #   X -> io_lib:format("~p", [X])
      # end
      binary_clause = :cerl.c_clause(
        [arg_var],
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:is_binary), [arg_var]),
        arg_var
      )
      atom_clause = :cerl.c_clause(
        [arg_var],
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:is_atom), [arg_var]),
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:atom_to_binary), [arg_var])
      )
      integer_clause = :cerl.c_clause(
        [arg_var],
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:is_integer), [arg_var]),
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:integer_to_binary), [arg_var])
      )
      float_clause = :cerl.c_clause(
        [arg_var],
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:is_float), [arg_var]),
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(:float_to_binary), [arg_var])
      )
      fallback_clause = :cerl.c_clause(
        [arg_var],
        :cerl.c_atom(true),
        :cerl.c_call(
          :cerl.c_atom(:erlang),
          :cerl.c_atom(:iolist_to_binary),
          [:cerl.c_call(
            :cerl.c_atom(:io_lib),
            :cerl.c_atom(:format),
            [:cerl.c_string(~c"~p"), :cerl.c_cons(arg_var, :cerl.c_nil())]
          )]
        )
      )

      :cerl.c_case(arg_core, [binary_clause, atom_clause, integer_clause, float_clause, fallback_clause])
    end)

    # Now concatenate all the converted strings
    # Build: erlang:iolist_to_binary([s1, s2, s3, ...])
    list_of_strings = Enum.reduce(Enum.reverse(converted_args), :cerl.c_nil(), fn elem, acc ->
      :cerl.c_cons(elem, acc)
    end)

    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:iolist_to_binary),
      [list_of_strings]
    )
  end

  # --- Send operations ---

  # Safe send (!) and unsafe send (!!) both compile to erlang:!/2
  # The difference is purely at type-checking time — at runtime they're identical.
  defp to_core_expr({:call, :"!", [pid_expr, msg_expr], _type}, user_fns, local_vars) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:!),
      [to_core_expr(pid_expr, user_fns, local_vars), to_core_expr(msg_expr, user_fns, local_vars)]
    )
  end

  defp to_core_expr({:call, :"!!", [pid_expr, msg_expr], _type}, user_fns, local_vars) do
    # !! compiles to the exact same thing as ! — erlang:!/2
    # The "unsafe" nature is a compile-time distinction only
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:!),
      [to_core_expr(pid_expr, user_fns, local_vars), to_core_expr(msg_expr, user_fns, local_vars)]
    )
  end

  # --- Anonymous functions ---

  # Anonymous function → Core Erlang fun
  defp to_core_expr({:fn, params, body, _type}, user_fns, local_vars) do
    # Extract parameter names to track as local variables
    fn_param_names = Enum.flat_map(params, fn
      {:tuple_pattern, _, _} = pattern -> Shared.extract_pattern_vars(pattern)
      {:var, name, _type} -> [name]
      atom when is_atom(atom) -> [atom]
      _ -> []
    end)

    # Function body sees its parameters as local variables
    fn_local_vars = MapSet.union(local_vars, MapSet.new(fn_param_names))

    # Check if any params have pattern destructuring
    has_patterns = Enum.any?(params, fn
      {:tuple_pattern, _, _} -> true
      {:var, _, _} -> true  # Already typed var
      atom when is_atom(atom) -> false
      _ -> true
    end)

    if has_patterns do
      # Generate unique arg names for pattern params
      {param_vars, patterns} = Enum.map_reduce(params, [], fn
        {:tuple_pattern, _, _} = pattern, acc ->
          arg_name = :"__arg_#{length(acc)}__"
          {:cerl.c_var(arg_name), [{arg_name, pattern} | acc]}
        {:var, name, _type}, acc ->
          {:cerl.c_var(name), acc}
        atom, acc when is_atom(atom) ->
          {:cerl.c_var(atom), acc}
        other, acc ->
          arg_name = :"__arg_#{length(acc)}__"
          {:cerl.c_var(arg_name), [{arg_name, other} | acc]}
      end)

      # Wrap body in case expressions for each pattern
      body_with_patterns = Enum.reduce(patterns, to_core_expr(body, user_fns, fn_local_vars), fn {arg_name, pattern}, acc ->
        core_pattern = to_core_pattern(pattern)
        clause = :cerl.c_clause([core_pattern], acc)
        :cerl.c_case(:cerl.c_var(arg_name), [clause])
      end)

      :cerl.c_fun(param_vars, body_with_patterns)
    else
      param_vars = Enum.map(params, &:cerl.c_var/1)
      body_core = to_core_expr(body, user_fns, fn_local_vars)
      :cerl.c_fun(param_vars, body_core)
    end
  end

  # --- Higher-order list functions ---

  # map: lists:map/2 with function reference (named function)
  defp to_core_expr({:call, :map, [func_name, list_expr], _type}, user_fns, local_vars) when is_atom(func_name) do
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    func_ref = :cerl.c_fname(func_name, 1)
    # Create a fun wrapper: fun(X) -> func_name(X) end
    x_var = :cerl.c_var(:__map_x__)
    fun_body = :cerl.c_apply(func_ref, [x_var])
    map_fun = :cerl.c_fun([x_var], fun_body)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:map),
      [map_fun, list_core]
    )
  end

  # map with anonymous function
  defp to_core_expr({:call, :map, [{:fn, _, _, _} = fn_ast, list_expr], _type}, user_fns, local_vars) do
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    fn_core = to_core_expr(fn_ast, user_fns, local_vars)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:map),
      [fn_core, list_core]
    )
  end

  # filter: lists:filter/2 with predicate reference (named function)
  defp to_core_expr({:call, :filter, [func_name, list_expr], _type}, user_fns, local_vars) when is_atom(func_name) do
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    func_ref = :cerl.c_fname(func_name, 1)
    x_var = :cerl.c_var(:__filter_x__)
    fun_body = :cerl.c_apply(func_ref, [x_var])
    filter_fun = :cerl.c_fun([x_var], fun_body)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:filter),
      [filter_fun, list_core]
    )
  end

  # filter with anonymous function
  defp to_core_expr({:call, :filter, [{:fn, _, _, _} = fn_ast, list_expr], _type}, user_fns, local_vars) do
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    fn_core = to_core_expr(fn_ast, user_fns, local_vars)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:filter),
      [fn_core, list_core]
    )
  end

  # Arithmetic/comparison operators that are BIFs, not user functions
  @bif_operators [:+, :-, :*, :/, :==, :!=, :<, :>, :<=, :>=]

  # fold: lists:foldl/3 with folder function (named function or BIF operator)
  defp to_core_expr({:call, :fold, [func_name, init_expr, list_expr], _type}, user_fns, local_vars) when is_atom(func_name) do
    init_core = to_core_expr(init_expr, user_fns, local_vars)
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    # lists:foldl expects fun(Elem, Acc) but we defined func(Acc, Elem)
    # so we need to swap the arguments
    elem_var = :cerl.c_var(:__fold_elem__)
    acc_var = :cerl.c_var(:__fold_acc__)

    # Create the fold function body depending on whether it's a BIF or user function
    fun_body = if func_name in @bif_operators do
      # BIF operators - call erlang:op(acc, elem)
      :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(func_name), [acc_var, elem_var])
    else
      # User-defined function - use c_fname
      func_ref = :cerl.c_fname(func_name, 2)
      :cerl.c_apply(func_ref, [acc_var, elem_var])
    end

    fold_fun = :cerl.c_fun([elem_var, acc_var], fun_body)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:foldl),
      [fold_fun, init_core, list_core]
    )
  end

  # fold with anonymous function
  # User writes (fn [acc elem] body) but foldl expects (elem, acc)
  defp to_core_expr({:call, :fold, [{:fn, params, body, fn_type}, init_expr, list_expr], _type}, user_fns, local_vars)
       when length(params) == 2 do
    init_core = to_core_expr(init_expr, user_fns, local_vars)
    list_core = to_core_expr(list_expr, user_fns, local_vars)
    # Swap params to match foldl convention
    [acc_param, elem_param] = params
    swapped_fn = {:fn, [elem_param, acc_param], body, fn_type}
    fn_core = to_core_expr(swapped_fn, user_fns, local_vars)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:foldl),
      [fn_core, init_core, list_core]
    )
  end

  defp to_core_expr({:call, :fold, [{:fn, params, _body, _fn_type}, _init_expr, _list_expr], _type}, _user_fns, _local_vars) do
    raise "fold function must take exactly 2 parameters (acc, elem), got #{length(params)}"
  end

  # Record construction → tagged tuple {:record_name, field1, field2, ...}
  defp to_core_expr({:call, name, args, {:record, name, _fields}}, user_fns, local_vars) do
    elements = [:cerl.c_atom(name) | Enum.map(args, &to_core_expr(&1, user_fns, local_vars))]
    :cerl.c_tuple(elements)
  end

  # Field access on record → erlang:element(index, tuple)
  # Records are stored as {:record_name, field1, field2, ...}
  # So field at position N (0-indexed) in field list is at tuple index N+2 (1-indexed, after tag)
  # New 5-element format: {:field_access, record_expr, field, field_type, record_type}
  defp to_core_expr({:field_access, record_expr, field, _field_type, _record_type}, user_fns, local_vars) do
    to_core_expr({:field_access, record_expr, field, :any}, user_fns, local_vars)
  end

  defp to_core_expr({:field_access, record_expr, field, _type}, user_fns, local_vars) do
    record_core = to_core_expr(record_expr, user_fns, local_vars)

    # Extract record type from the typed expression to find field index
    case extract_type(record_expr) do
      {:record, _name, fields} ->
        # Find field index in the field list
        field_index = Enum.find_index(fields, fn {f, _type} -> f == field end)
        # Tuple index: 1-indexed, first element is the record tag
        tuple_index = field_index + 2
        :cerl.c_call(
          :cerl.c_atom(:erlang),
          :cerl.c_atom(:element),
          [:cerl.c_int(tuple_index), record_core]
        )

      {:row, _fields, _tail} ->
        # Row types are maps - use maps:get
        :cerl.c_call(
          :cerl.c_atom(:maps),
          :cerl.c_atom(:get),
          [:cerl.c_atom(field), record_core]
        )

      _other ->
        # Unknown type - fall back to maps:get (for interop with Erlang maps)
        :cerl.c_call(
          :cerl.c_atom(:maps),
          :cerl.c_atom(:get),
          [:cerl.c_atom(field), record_core]
        )
    end
  end



  # Qualified sum type constructor: (Module/Constructor args) where the result type is a sum type
  # and the func name is one of the constructors → emit tagged tuple
  defp to_core_expr({:call, {:qualified, mod, ctor}, args, {:sum, _sum_name, variants}}, user_fns, local_vars)
       when is_atom(ctor) do
    if List.keymember?(variants, ctor, 0) do
      # This is a sum type constructor - emit tagged tuple {:Constructor, args...}
      elements = [:cerl.c_atom(ctor) | Enum.map(args, &to_core_expr(&1, user_fns, local_vars))]
      :cerl.c_tuple(elements)
    else
      # Regular qualified call that happens to return a sum type
      arg_cores = Enum.map(args, &to_core_expr(&1, user_fns, local_vars))
      :cerl.c_call(
        :cerl.c_atom(elixir_module_name(mod)),
        :cerl.c_atom(ctor),
        arg_cores
      )
    end
  end

  # Qualified call: (Module/func args) → :cerl.c_call(Module, func, args)
  defp to_core_expr({:call, {:qualified, mod, func}, args, _type}, user_fns, local_vars) do
    arg_cores = Enum.map(args, &to_core_expr(&1, user_fns, local_vars))
    :cerl.c_call(
      :cerl.c_atom(elixir_module_name(mod)),
      :cerl.c_atom(func),
      arg_cores
    )
  end

  # Variant construction → tagged tuple {:VariantName, field1, field2, ...}
  # Only when the call name is an actual constructor of the sum type
  defp to_core_expr({:call, ctor_name, args, {:sum, _sum_name, variants}}, user_fns, local_vars) do
    if List.keymember?(variants, ctor_name, 0) do
      # Variant constructor: emit tagged tuple
      elements = [:cerl.c_atom(ctor_name) | Enum.map(args, &to_core_expr(&1, user_fns, local_vars))]
      :cerl.c_tuple(elements)
    else
      # Regular function call that returns a sum type
      arity = length(args)
      arg_cores = Enum.map(args, &to_core_expr(&1, user_fns, local_vars))

      if MapSet.member?(user_fns, {ctor_name, arity}) do
        :cerl.c_apply(:cerl.c_fname(ctor_name, arity), arg_cores)
      else
        :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(ctor_name), arg_cores)
      end
    end
  end

  # Apply: calling a function stored in a variable
  # The type checker emits {:apply, {:var, name, type}, args, ret_type} for local fn vars
  defp to_core_expr({:apply, {:var, func_name, _func_type}, args, _type}, user_fns, local_vars) do
    arg_cores = Enum.map(args, &to_core_expr(&1, user_fns, local_vars))
    # Use c_apply with the variable - this invokes the function value stored in the variable
    :cerl.c_apply(:cerl.c_var(func_name), arg_cores)
  end

  # User-defined function call → local apply
  # This clause handles calls that didn't match any specialized patterns above
  defp to_core_expr({:call, func, args, _type}, user_fns, local_vars) do
    arity = length(args)
    arg_cores = Enum.map(args, &to_core_expr(&1, user_fns, local_vars))

    cond do
      # Call to a local variable (function passed as parameter or let-bound)
      MapSet.member?(local_vars, func) ->
        # Use c_apply with the variable - this invokes the function value stored in the variable
        :cerl.c_apply(:cerl.c_var(func), arg_cores)

      # Call to a user-defined function in this module
      MapSet.member?(user_fns, {func, arity}) ->
        :cerl.c_apply(:cerl.c_fname(func, arity), arg_cores)

      # External call (erlang BIFs or other modules)
      true ->
        :cerl.c_call(
          :cerl.c_atom(:erlang),
          :cerl.c_atom(func),
          arg_cores
        )
    end
  end

  # Fallback for raw literals (from parser)
  defp to_core_expr(n, _user_fns, _local_vars) when is_integer(n), do: :cerl.c_int(n)
  defp to_core_expr(f, _user_fns, _local_vars) when is_float(f), do: :cerl.c_float(f)
  defp to_core_expr(a, _user_fns, _local_vars) when is_atom(a), do: :cerl.c_atom(a)

  # --- Helper functions ---

  # Extract type from a typed AST node
  defp extract_type({:var, _name, type}), do: type
  defp extract_type({:call, _func, _args, type}), do: type
  defp extract_type({:field_access, _expr, _field, field_type, _record_type}), do: field_type
  defp extract_type({:field_access, _expr, _field, type}), do: type
  defp extract_type({:let, _bindings, _body, type}), do: type
  defp extract_type({:if, _cond, _then, _else, type}), do: type
  defp extract_type({:match, _expr, _clauses, type}), do: type
  defp extract_type({:lit, type, _value}), do: type
  defp extract_type({:map, _pairs, type}), do: type
  defp extract_type(_), do: :any

  # Simple variable binding
  defp emit_let_binding({name, expr, _type}, body, user_fns, local_vars) when is_atom(name) do
    var = :cerl.c_var(name)
    value = to_core_expr(expr, user_fns, local_vars)
    :cerl.c_let([var], value, body)
  end

  # Tuple pattern destructuring - use case expression
  defp emit_let_binding({{:tuple_pattern, _, _} = pattern, expr, _type}, body, user_fns, local_vars) do
    value = to_core_expr(expr, user_fns, local_vars)
    core_pattern = to_core_pattern(pattern)
    clause = :cerl.c_clause([core_pattern], body)
    :cerl.c_case(value, [clause])
  end

  # Cons pattern destructuring: (let [[head | tail] expr] ...)
  defp emit_let_binding({{:cons_pattern, head, tail, _type}, expr, _expr_type}, body, user_fns, local_vars) do
    value = to_core_expr(expr, user_fns, local_vars)
    core_pattern = :cerl.c_cons(:cerl.c_var(head), :cerl.c_var(tail))
    clause = :cerl.c_clause([core_pattern], body)
    :cerl.c_case(value, [clause])
  end

  # --- Pattern transformation ---

  # Record pattern → tuple pattern {:record_name, var1, var2, ...}
  defp to_core_pattern({:pattern, name, args, _type}) when is_atom(name) do
    pattern_args = Enum.map(args, &to_core_pattern/1)
    :cerl.c_tuple([:cerl.c_atom(name) | pattern_args])
  end

  # Qualified constructor pattern: (Mod/Ctor arg1 arg2)
  defp to_core_pattern({:pattern, {:qualified, mod, ctor}, args, _type}) do
    pattern_args = Enum.map(args, &to_core_pattern/1)
    # Use the full qualified name as the tag: :"Mod.Ctor"
    tag = String.to_atom("#{mod}.#{ctor}")
    :cerl.c_tuple([:cerl.c_atom(tag) | pattern_args])
  end

  defp to_core_pattern({:var, name, _type}) do
    :cerl.c_var(name)
  end

  # Atom literal pattern: {:lit, :atom, value}
  defp to_core_pattern({:lit, :atom, value}) do
    :cerl.c_atom(value)
  end

  # Tuple pattern: {:tuple_pattern, elements, type}
  defp to_core_pattern({:tuple_pattern, elements, _type}) do
    pattern_elements = Enum.map(elements, &to_core_pattern/1)
    :cerl.c_tuple(pattern_elements)
  end

  # Tuple pattern without type annotation (from anonymous function patterns)
  defp to_core_pattern({:tuple_pattern, elements}) when is_list(elements) do
    pattern_elements = Enum.map(elements, &to_core_pattern/1)
    :cerl.c_tuple(pattern_elements)
  end

  # Empty list pattern
  defp to_core_pattern({:list_pattern, [], _type}) do
    :cerl.c_nil()
  end

  # Non-empty list pattern: [a, b, c] → cons(a, cons(b, cons(c, nil)))
  defp to_core_pattern({:list_pattern, elements, _type}) do
    List.foldr(elements, :cerl.c_nil(), fn el, acc ->
      :cerl.c_cons(to_core_pattern(el), acc)
    end)
  end

  # Cons pattern: {:cons_pattern, head, tail, type}
  defp to_core_pattern({:cons_pattern, head, tail, _type}) do
    head_pattern = to_core_pattern(head)
    tail_pattern = to_core_pattern(tail)
    :cerl.c_cons(head_pattern, tail_pattern)
  end

  defp to_core_pattern(n) when is_integer(n), do: :cerl.c_int(n)

  # Wildcard pattern - generate a unique variable name for each wildcard
  # Core Erlang doesn't allow multiple _ variables in the same pattern
  defp to_core_pattern(:_) do
    # Generate a unique name for this wildcard using System.unique_integer
    unique_id = System.unique_integer([:positive])
    :cerl.c_var(:"_#{unique_id}")
  end

  defp to_core_pattern(a) when is_atom(a), do: :cerl.c_atom(a)

  # String pattern - convert to bitstring pattern matching the entire string as an integer
  # This matches how Erlang compiles <<"hello">> patterns to Core Erlang
  defp to_core_pattern({:string, s, _type}) when is_binary(s) do
    # Convert string to big-endian integer (how Erlang represents binaries in patterns)
    int_value = s |> :binary.bin_to_list() |> Enum.reduce(0, fn byte, acc -> acc * 256 + byte end)
    bit_size = byte_size(s) * 8

    # Build bitstring segment: value, size, unit, type, flags
    segment = :cerl.c_bitstr(
      :cerl.c_int(int_value),
      :cerl.c_int(bit_size),
      :cerl.c_int(1),
      :cerl.c_atom(:integer),
      :cerl.c_cons(:cerl.c_atom(:unsigned), :cerl.c_cons(:cerl.c_atom(:big), :cerl.c_nil()))
    )

    :cerl.c_binary([segment])
  end

  # --- Multi-clause function helpers ---

  # Convert multi-clause patterns to Core Erlang
  # Empty list: [] → []
  defp to_core_multi_pattern({:list, [], _type}), do: :cerl.c_nil()

  # List with elements
  defp to_core_multi_pattern({:list, elements, _type}) do
    elements
    |> Enum.map(&to_core_multi_pattern/1)
    |> Enum.reverse()
    |> Enum.reduce(:cerl.c_nil(), fn elem, acc -> :cerl.c_cons(elem, acc) end)
  end

  # Cons pattern: [h | t] → cons(h, t)
  defp to_core_multi_pattern({:cons, head, tail, _type}) do
    :cerl.c_cons(to_core_multi_pattern(head), to_core_multi_pattern(tail))
  end

  # Variable in pattern
  defp to_core_multi_pattern({:var, name, _type}), do: :cerl.c_var(name)

  # Literals
  defp to_core_multi_pattern({:lit, :int, n}), do: :cerl.c_int(n)
  defp to_core_multi_pattern({:lit, :atom, a}), do: :cerl.c_atom(a)
  defp to_core_multi_pattern({:lit, :bool, b}), do: :cerl.c_atom(b)

  # Underscore (wildcard) - generate unique variable name
  defp to_core_multi_pattern(:_) do
    unique_id = System.unique_integer([:positive])
    :cerl.c_var(:"_#{unique_id}")
  end
  defp to_core_multi_pattern(a) when is_atom(a), do: :cerl.c_var(a)
  defp to_core_multi_pattern(n) when is_integer(n), do: :cerl.c_int(n)

  # --- String helpers ---

  # Convert Elixir string to Core Erlang binary segments
  defp string_to_binary_segments(s) do
    s
    |> :binary.bin_to_list()
    |> Enum.map(fn byte ->
      :cerl.c_bitstr(
        :cerl.c_int(byte),
        :cerl.c_int(8),
        :cerl.c_int(1),
        :cerl.c_atom(:integer),
        :cerl.c_cons(:cerl.c_atom(:unsigned), :cerl.c_cons(:cerl.c_atom(:big), :cerl.c_nil()))
      )
    end)
  end

  # --- Error formatting ---

  defp format_errors(errors) do
    errors
    |> Enum.map(fn {_file, file_errors} ->
      Enum.map(file_errors, fn {_line, _mod, msg} ->
        :erlang.iolist_to_binary(:compile.format_error(msg))
      end)
    end)
    |> List.flatten()
    |> Enum.join("\n")
  end

  # --- Module name resolution ---
  # Vaisto/Elixir modules need Elixir. prefix, Erlang modules don't

  defp elixir_module_name(mod) when is_atom(mod) do
    mod_str = Atom.to_string(mod)

    cond do
      # Already has Elixir. prefix
      String.starts_with?(mod_str, "Elixir.") ->
        mod

      # Vaisto module: starts with uppercase or contains a dot
      vaisto_module?(mod_str) ->
        String.to_atom("Elixir.#{mod_str}")

      # Erlang module (lowercase, no dots)
      true ->
        mod
    end
  end

  defp vaisto_module?(mod_str) do
    # A Vaisto module starts with uppercase or contains a dot
    String.contains?(mod_str, ".") or
      (String.length(mod_str) > 0 and
         String.first(mod_str) == String.upcase(String.first(mod_str)) and
         String.first(mod_str) =~ ~r/[A-Z]/)
  end
end
