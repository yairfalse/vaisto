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

  # Literals
  defp to_core_expr({:lit, :int, n}), do: :cerl.c_int(n)
  defp to_core_expr({:lit, :float, f}), do: :cerl.c_float(f)
  defp to_core_expr({:lit, :atom, a}), do: :cerl.c_atom(a)
  defp to_core_expr({:lit, :bool, b}), do: :cerl.c_atom(b)

  # Variables
  defp to_core_expr({:var, name, _type}) do
    :cerl.c_var(name)
  end

  # Arithmetic: (+ a b) → erlang:'+'(a, b)
  defp to_core_expr({:call, op, [left, right], _type}) when op in [:+, :-, :*, :/] do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(op),
      [to_core_expr(left), to_core_expr(right)]
    )
  end

  # Comparison operators
  defp to_core_expr({:call, op, [left, right], _type}) when op in [:==, :!=, :<, :>, :<=, :>=] do
    erlang_op = case op do
      :== -> :"=:="
      :!= -> :"/="
      _ -> op
    end

    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(erlang_op),
      [to_core_expr(left), to_core_expr(right)]
    )
  end

  # Generic function call
  defp to_core_expr({:call, func, args, _type}) do
    :cerl.c_call(
      :cerl.c_atom(:erlang),
      :cerl.c_atom(func),
      Enum.map(args, &to_core_expr/1)
    )
  end

  # Fallback for raw literals (from parser)
  defp to_core_expr(n) when is_integer(n), do: :cerl.c_int(n)
  defp to_core_expr(f) when is_float(f), do: :cerl.c_float(f)
  defp to_core_expr(a) when is_atom(a), do: :cerl.c_atom(a)

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
