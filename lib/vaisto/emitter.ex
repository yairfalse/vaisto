defmodule Vaisto.Emitter do
  @moduledoc """
  Emits Elixir AST from typed Vaisto AST.

  Strategy: Vaisto AST → Elixir AST → BEAM

  This leverages Elixir's compiler instead of targeting Core Erlang
  directly. More sustainable, better documented, battle-tested.
  """

  @doc """
  Transform typed Vaisto AST to Elixir AST (quoted form).

  ## Example

      iex> Emitter.to_elixir({:lit, :int, 42})
      42

      iex> Emitter.to_elixir({:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int})
      {:+, [], [1, 2]}
  """
  def to_elixir(typed_ast)

  # Literals pass through
  def to_elixir({:lit, :int, n}), do: n
  def to_elixir({:lit, :float, f}), do: f
  def to_elixir({:lit, :bool, b}), do: b
  def to_elixir({:lit, :atom, a}), do: a
  def to_elixir({:lit, :string, s}), do: s

  # List literal
  def to_elixir({:list, elements, _type}) do
    Enum.map(elements, &to_elixir/1)
  end

  # Variables become Elixir variables
  # Using Macro.var ensures proper hygiene in quote blocks
  def to_elixir({:var, name, _type}) do
    Macro.var(name, nil)
  end

  # If expression → Elixir if
  def to_elixir({:if, condition, then_branch, else_branch, _type}) do
    cond_ast = to_elixir(condition)
    then_ast = to_elixir(then_branch)
    else_ast = to_elixir(else_branch)

    quote do
      if unquote(cond_ast) do
        unquote(then_ast)
      else
        unquote(else_ast)
      end
    end
  end

  # Match expression → Elixir case
  def to_elixir({:match, expr, clauses, _type}) do
    expr_ast = to_elixir(expr)
    clause_asts = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_ast = emit_pattern(pattern)
      body_ast = to_elixir(body)
      {:->, [], [[pattern_ast], body_ast]}
    end)

    {:case, [], [expr_ast, [do: clause_asts]]}
  end

  # Let bindings → nested assignments using Elixir's block
  # (let [x 1 y 2] (+ x y)) → (x = 1; y = 2; x + y)
  def to_elixir({:let, bindings, body, _type}) do
    body_ast = to_elixir(body)

    # Build assignments from innermost to outermost
    List.foldr(bindings, body_ast, fn {name, expr, _type}, acc ->
      var = Macro.var(name, nil)
      value = to_elixir(expr)
      quote do
        unquote(var) = unquote(value)
        unquote(acc)
      end
    end)
  end

  # Arithmetic calls → Elixir operators
  def to_elixir({:call, op, [left, right], _type}) when op in [:+, :-, :*, :/] do
    {op, [], [to_elixir(left), to_elixir(right)]}
  end

  # Comparison calls
  def to_elixir({:call, op, [left, right], _type}) when op in [:==, :!=, :<, :>, :<=, :>=] do
    {op, [], [to_elixir(left), to_elixir(right)]}
  end

  # spawn: start a GenServer and return its PID
  # (spawn counter 0) → Counter.start_link(0) |> elem(1)
  def to_elixir({:call, :spawn, [process_name, init_arg], _pid_type}) do
    module = camelize(process_name)
    init = to_elixir(init_arg)

    quote do
      {:ok, pid} = unquote(module).start_link(unquote(init))
      pid
    end
  end

  # send (!): call the GenServer with a message
  # (! pid :increment) → GenServer.call(pid, :increment)
  def to_elixir({:call, :"!", [pid_expr, msg_expr], _type}) do
    pid = to_elixir(pid_expr)
    msg = to_elixir(msg_expr)

    quote do
      GenServer.call(unquote(pid), unquote(msg))
    end
  end

  # Record type definition - no runtime code needed, just documentation
  def to_elixir({:deftype, _name, _fields, _type}) do
    # deftype is a compile-time construct, no runtime representation needed
    # Could emit a struct definition in the future
    nil
  end

  # Function definition → Elixir def
  def to_elixir({:defn, name, params, body, _type}) do
    param_vars = Enum.map(params, &Macro.var(&1, nil))
    body_ast = to_elixir(body)

    quote do
      def unquote(name)(unquote_splicing(param_vars)) do
        unquote(body_ast)
      end
    end
  end

  # Record construction → tagged tuple {:record_name, field1, field2, ...}
  def to_elixir({:call, name, args, {:record, name, _fields}}) do
    typed_args = Enum.map(args, &to_elixir/1)
    {:{}, [], [name | typed_args]}
  end

  # Generic function call
  def to_elixir({:call, func, args, _type}) do
    {func, [], Enum.map(args, &to_elixir/1)}
  end

  # Process definition → GenServer module
  def to_elixir({:process, name, initial_state, handlers, _type}) do
    emit_genserver(name, initial_state, handlers)
  end

  # Supervision tree → Supervisor module
  def to_elixir({:supervise, strategy, children}) do
    emit_supervisor(strategy, children)
  end

  # Module: collection of definitions
  def to_elixir({:module, forms}) do
    forms
    |> Enum.map(&to_elixir/1)
    |> Enum.reject(&is_nil/1)
  end

  # Fallback for raw values (untyped literals from parser)
  def to_elixir(n) when is_integer(n), do: n
  def to_elixir(f) when is_float(f), do: f
  def to_elixir(a) when is_atom(a), do: a

  @doc """
  Compile typed AST to a module and return the bytecode.

  Returns {:ok, module_name, bytecode} or {:error, reason}
  For modules: returns {:ok, :module, [{module_name, bytecode}, ...]}
  """
  def compile(typed_ast, module_name \\ :vaisto_module)

  # Module compilation - produces multiple modules (processes, supervisors)
  # or a single module with user-defined functions + main
  def compile({:module, _forms} = module_ast, module_name) do
    elixir_asts = to_elixir(module_ast)

    # Separate standalone modules (GenServers, Supervisors) from function defs
    {standalone_modules, fn_defs_and_exprs} = Enum.split_with(elixir_asts, fn ast ->
      match?({:defmodule, _, _}, ast)
    end)

    try do
      # Compile standalone modules first
      standalone_results = Enum.flat_map(standalone_modules, fn ast ->
        Code.compile_quoted(ast)
      end)

      # If there are function defs or expressions, wrap them in a module
      main_results = case fn_defs_and_exprs do
        [] -> []
        items ->
          # Separate defn-generated defs from expression results
          {defs, exprs} = Enum.split_with(items, fn
            {:def, _, _} -> true
            _ -> false
          end)

          # Build main function from expressions (if any)
          main_def = case exprs do
            [] -> []
            [single] ->
              [quote do
                def main do
                  unquote(single)
                end
              end]
            multiple ->
              # Combine multiple expressions into a block, return last
              [quote do
                def main do
                  unquote_splicing(multiple)
                end
              end]
          end

          module_ast = quote do
            defmodule unquote(module_name) do
              unquote_splicing(defs ++ main_def)
            end
          end

          Code.compile_quoted(module_ast)
      end

      {:ok, module_name, main_results ++ standalone_results}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  # Process definition - compile directly to GenServer module
  def compile({:process, name, _init, _handlers, _type} = process_ast, _module_name) do
    elixir_ast = to_elixir(process_ast)
    module = camelize(name)

    try do
      [{^module, bytecode}] = Code.compile_quoted(elixir_ast)
      {:ok, module, bytecode}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  # Single expression compilation - wrap in module
  def compile(typed_ast, module_name) do
    elixir_ast = to_elixir(typed_ast)

    # Wrap expression in a module with a main/0 function
    module_ast = wrap_in_module(elixir_ast, module_name)

    try do
      [{^module_name, bytecode}] = Code.compile_quoted(module_ast)
      {:ok, module_name, bytecode}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  # Private helpers

  defp wrap_in_module(expr_ast, module_name) do
    quote do
      defmodule unquote(module_name) do
        def main do
          unquote(expr_ast)
        end
      end
    end
  end

  defp emit_genserver(name, _initial_state, handlers) do
    module_name = camelize(name)
    handle_clauses = Enum.map(handlers, &emit_handle_call/1)

    quote do
      defmodule unquote(module_name) do
        use GenServer

        def start_link(init_arg) do
          GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
        end

        @impl true
        def init(state), do: {:ok, state}

        unquote_splicing(handle_clauses)
      end
    end
  end

  defp emit_handle_call({msg_pattern, body_ast, _ret_type}) do
    msg = emit_pattern(msg_pattern)
    body = to_elixir(body_ast)

    # Use var! to escape hygiene and access the function parameter
    quote do
      @impl true
      def handle_call(unquote(msg), _from, var!(state)) do
        new_state = unquote(body)
        {:reply, new_state, new_state}
      end
    end
  end

  # Record pattern → tagged tuple pattern {:record_name, var1, var2, ...}
  defp emit_pattern({:pattern, name, args, _type}) do
    pattern_args = Enum.map(args, &emit_pattern/1)
    {:{}, [], [name | pattern_args]}
  end

  defp emit_pattern({:var, name, _type}) do
    Macro.var(name, nil)
  end

  defp emit_pattern({:lit, :atom, a}), do: a
  defp emit_pattern(a) when is_atom(a), do: a
  defp emit_pattern(n) when is_integer(n), do: n

  defp emit_supervisor(strategy, children) do
    # Build child specs at compile time
    child_specs = Enum.map(children, fn child ->
      {module, init_arg} = child_spec_tuple(child)
      quote do: %{id: unquote(module), start: {unquote(module), :start_link, [unquote(init_arg)]}}
    end)

    quote do
      defmodule VaistoSupervisor do
        use Supervisor

        def start_link(init_arg) do
          Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
        end

        @impl true
        def init(_init_arg) do
          children = unquote(child_specs)
          Supervisor.init(children, strategy: unquote(strategy))
        end
      end
    end
  end

  defp child_spec_tuple({:call, name, args, _type}) do
    module = camelize(name)
    init_arg = case args do
      [arg] -> to_elixir(arg)
      args -> Enum.map(args, &to_elixir/1)
    end
    {module, init_arg}
  end

  defp child_spec_tuple({:call, name, args}) do
    module = camelize(name)
    init_arg = case args do
      [arg] -> to_elixir(arg)
      args -> Enum.map(args, &to_elixir/1)
    end
    {module, init_arg}
  end


  defp camelize(atom) when is_atom(atom) do
    name =
      atom
      |> Atom.to_string()
      |> String.split("_")
      |> Enum.map(&String.capitalize/1)
      |> Enum.join()

    # Use Module.concat to get proper Elixir module atom
    Module.concat([String.to_atom(name)])
  end
end
