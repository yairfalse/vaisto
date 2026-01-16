defmodule Vaisto.CoreEmitter do
  @moduledoc """
  Emits Core Erlang AST from typed Vaisto AST and compiles to BEAM.

  This is the "deep" path - direct compilation to BEAM bytecode
  without going through Elixir. More control, tighter output.

  Core Erlang is built using the :cerl module which constructs
  the AST nodes that :compile.forms/2 understands.
  """

  @doc """
  Compile typed AST directly to BEAM bytecode.

  Returns {:ok, module_name, binary} or {:error, reason}
  """
  def compile(typed_ast, module_name \\ :VaistoModule) do
    core_ast = to_core(typed_ast, module_name)

    case :compile.forms(core_ast, [:from_core, :binary, :return_errors]) do
      {:ok, ^module_name, binary} ->
        {:ok, module_name, binary}

      {:error, errors, _warnings} ->
        {:error, format_errors(errors)}
    end
  end

  @doc """
  Compile and load the module into the VM.
  """
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
  def to_core(typed_ast, module_name)

  # Process definition → raw BEAM process module
  def to_core({:process, _name, _init, handlers, _type}, module_name) do
    to_core_process(module_name, handlers)
  end

  # Module with function definitions
  def to_core({:module, forms}, module_name) do
    # Separate defn forms from other expressions
    {defns, exprs} = Enum.split_with(forms, fn
      {:defn, _, _, _, _} -> true
      {:deftype, _, _, _} -> true  # Skip deftypes (compile-time only)
      _ -> false
    end)

    # Filter out deftypes, keep only defns
    defns = Enum.filter(defns, &match?({:defn, _, _, _, _}, &1))

    # Track user-defined function names for local calls
    user_fns = MapSet.new(defns, fn {:defn, name, params, _, _} -> {name, length(params)} end)

    # Build function definitions
    fun_defs = Enum.map(defns, fn {:defn, name, params, body, _type} ->
      param_vars = Enum.map(params, &:cerl.c_var/1)
      body_core = to_core_expr(body, user_fns)
      fun = :cerl.c_fun(param_vars, body_core)
      fname = :cerl.c_fname(name, length(params))
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

    # Export all user-defined functions plus main
    fun_exports = Enum.map(fun_defs, fn {fname, _} -> fname end)

    :cerl.c_module(
      :cerl.c_atom(module_name),
      fun_exports ++ main_exports,
      [],
      fun_defs ++ main_defs
    )
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
    # Pattern: {msg, From}
    pattern = :cerl.c_tuple([:cerl.c_atom(msg), from_var])

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
  # Default: no user functions context
  defp to_core_expr(ast), do: to_core_expr(ast, MapSet.new())

  # Literals
  defp to_core_expr({:lit, :int, n}, _user_fns), do: :cerl.c_int(n)
  defp to_core_expr({:lit, :float, f}, _user_fns), do: :cerl.c_float(f)
  defp to_core_expr({:lit, :atom, a}, _user_fns), do: :cerl.c_atom(a)
  defp to_core_expr({:lit, :bool, b}, _user_fns), do: :cerl.c_atom(b)
  defp to_core_expr({:lit, :string, s}, _user_fns), do: :cerl.c_binary(string_to_binary_segments(s))

  # List literal
  defp to_core_expr({:list, elements, _type}, user_fns) do
    elements
    |> Enum.map(&to_core_expr(&1, user_fns))
    |> Enum.reverse()
    |> Enum.reduce(:cerl.c_nil(), fn elem, acc -> :cerl.c_cons(elem, acc) end)
  end

  # Variables
  defp to_core_expr({:var, name, _type}, _user_fns) do
    :cerl.c_var(name)
  end

  # If expression → Core Erlang case on boolean
  defp to_core_expr({:if, condition, then_branch, else_branch, _type}, user_fns) do
    cond_core = to_core_expr(condition, user_fns)
    then_core = to_core_expr(then_branch, user_fns)
    else_core = to_core_expr(else_branch, user_fns)

    # case Cond of true -> Then; false -> Else end
    true_clause = :cerl.c_clause([:cerl.c_atom(true)], :cerl.c_atom(true), then_core)
    false_clause = :cerl.c_clause([:cerl.c_atom(false)], :cerl.c_atom(true), else_core)

    :cerl.c_case(cond_core, [true_clause, false_clause])
  end

  # Match expression → Core Erlang case
  defp to_core_expr({:match, expr, clauses, _type}, user_fns) do
    expr_core = to_core_expr(expr, user_fns)
    clause_cores = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_core = to_core_pattern(pattern)
      body_core = to_core_expr(body, user_fns)
      :cerl.c_clause([pattern_core], :cerl.c_atom(true), body_core)
    end)
    :cerl.c_case(expr_core, clause_cores)
  end

  # Let bindings: nest each binding as Core Erlang let
  # (let [x 1 y 2] body) → let x = 1 in let y = 2 in body
  defp to_core_expr({:let, bindings, body, _type}, user_fns) do
    body_expr = to_core_expr(body, user_fns)

    # Build from innermost to outermost
    List.foldr(bindings, body_expr, fn {name, expr, _type}, acc ->
      var = :cerl.c_var(name)
      value = to_core_expr(expr, user_fns)
      :cerl.c_let([var], value, acc)
    end)
  end

  # Arithmetic: (+ a b) → erlang:'+'(a, b)
  defp to_core_expr({:call, op, [left, right], _type}, user_fns) when op in [:+, :-, :*, :/] do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(op),
      [to_core_expr(left, user_fns), to_core_expr(right, user_fns)]
    )
  end

  # Comparison operators
  defp to_core_expr({:call, op, [left, right], _type}, user_fns) when op in [:==, :!=, :<, :>, :<=, :>=] do
    erlang_op = case op do
      :== -> :"=:="
      :!= -> :"/="
      _ -> op
    end

    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(erlang_op),
      [to_core_expr(left, user_fns), to_core_expr(right, user_fns)]
    )
  end

  # --- List operations ---

  # head: erlang:hd/1
  defp to_core_expr({:call, :head, [list_expr], _type}, user_fns) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:hd),
      [to_core_expr(list_expr, user_fns)]
    )
  end

  # tail: erlang:tl/1
  defp to_core_expr({:call, :tail, [list_expr], _type}, user_fns) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:tl),
      [to_core_expr(list_expr, user_fns)]
    )
  end

  # cons: [elem | list]
  defp to_core_expr({:call, :cons, [elem_expr, list_expr], _type}, user_fns) do
    :cerl.c_cons(
      to_core_expr(elem_expr, user_fns),
      to_core_expr(list_expr, user_fns)
    )
  end

  # empty?: list == []
  defp to_core_expr({:call, :empty?, [list_expr], _type}, user_fns) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:"=:="),
      [to_core_expr(list_expr, user_fns), :cerl.c_nil()]
    )
  end

  # length: erlang:length/1
  defp to_core_expr({:call, :length, [list_expr], _type}, user_fns) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(:length),
      [to_core_expr(list_expr, user_fns)]
    )
  end

  # --- Higher-order list functions ---

  # map: lists:map/2 with function reference
  defp to_core_expr({:call, :map, [func_name, list_expr], _type}, user_fns) when is_atom(func_name) do
    list_core = to_core_expr(list_expr, user_fns)
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

  # filter: lists:filter/2 with predicate reference
  defp to_core_expr({:call, :filter, [func_name, list_expr], _type}, user_fns) when is_atom(func_name) do
    list_core = to_core_expr(list_expr, user_fns)
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

  # fold: lists:foldl/3 with folder function
  defp to_core_expr({:call, :fold, [func_name, init_expr, list_expr], _type}, user_fns) when is_atom(func_name) do
    init_core = to_core_expr(init_expr, user_fns)
    list_core = to_core_expr(list_expr, user_fns)
    func_ref = :cerl.c_fname(func_name, 2)
    # lists:foldl expects fun(Elem, Acc) but we defined func(Acc, Elem)
    # so we need to swap the arguments
    elem_var = :cerl.c_var(:__fold_elem__)
    acc_var = :cerl.c_var(:__fold_acc__)
    fun_body = :cerl.c_apply(func_ref, [acc_var, elem_var])
    fold_fun = :cerl.c_fun([elem_var, acc_var], fun_body)
    :cerl.c_call(
      :cerl.c_atom(:lists),
      :cerl.c_atom(:foldl),
      [fold_fun, init_core, list_core]
    )
  end

  # Record construction → tagged tuple {:record_name, field1, field2, ...}
  defp to_core_expr({:call, name, args, {:record, name, _fields}}, user_fns) do
    elements = [:cerl.c_atom(name) | Enum.map(args, &to_core_expr(&1, user_fns))]
    :cerl.c_tuple(elements)
  end

  # User-defined function call → local apply
  defp to_core_expr({:call, func, args, _type}, user_fns) do
    arity = length(args)
    arg_cores = Enum.map(args, &to_core_expr(&1, user_fns))

    if MapSet.member?(user_fns, {func, arity}) do
      # Local function call via apply
      :cerl.c_apply(:cerl.c_fname(func, arity), arg_cores)
    else
      # External call (erlang BIFs)
      :cerl.c_call(
        :cerl.c_atom(:erlang),
        :cerl.c_atom(func),
        arg_cores
      )
    end
  end

  # Fallback for raw literals (from parser)
  defp to_core_expr(n, _user_fns) when is_integer(n), do: :cerl.c_int(n)
  defp to_core_expr(f, _user_fns) when is_float(f), do: :cerl.c_float(f)
  defp to_core_expr(a, _user_fns) when is_atom(a), do: :cerl.c_atom(a)

  # --- Pattern transformation ---

  # Record pattern → tuple pattern {:record_name, var1, var2, ...}
  defp to_core_pattern({:pattern, name, args, _type}) do
    pattern_args = Enum.map(args, &to_core_pattern/1)
    :cerl.c_tuple([:cerl.c_atom(name) | pattern_args])
  end

  defp to_core_pattern({:var, name, _type}) do
    :cerl.c_var(name)
  end

  defp to_core_pattern(n) when is_integer(n), do: :cerl.c_int(n)
  defp to_core_pattern(a) when is_atom(a), do: :cerl.c_atom(a)

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
end
