defmodule Vaisto.Emitter do
  @moduledoc """
  Emits Elixir AST from typed Vaisto AST.

  Strategy: Vaisto AST → Elixir AST → BEAM

  This leverages Elixir's compiler instead of targeting Core Erlang
  directly. More sustainable, better documented, battle-tested.
  """

  alias Vaisto.Error
  alias Vaisto.TypeChecker

  @doc """
  Transform typed Vaisto AST to Elixir AST (quoted form).

  ## Example

      iex> Emitter.to_elixir({:lit, :int, 42})
      42

      iex> Emitter.to_elixir({:call, :+, [{:lit, :int, 1}, {:lit, :int, 2}], :int})
      {:+, [], [1, 2]}
  """
  @spec to_elixir(TypeChecker.typed_ast() | integer() | float() | atom()) :: Macro.t()
  def to_elixir(typed_ast)

  # Literals pass through
  def to_elixir({:lit, :int, n}), do: n
  def to_elixir({:lit, :float, f}), do: f
  def to_elixir({:lit, :bool, b}), do: b
  def to_elixir({:lit, :atom, a}), do: a
  def to_elixir({:lit, :string, s}), do: s
  def to_elixir({:lit, :unit, _}), do: nil

  # List literal
  def to_elixir({:list, elements, _type}) do
    Enum.map(elements, &to_elixir/1)
  end

  # Variables become Elixir variables
  # Using Macro.var ensures proper hygiene in quote blocks
  def to_elixir({:var, name, _type}) do
    Macro.var(name, nil)
  end

  # Function reference - module-level function passed as value
  # Becomes &name/arity in Elixir
  def to_elixir({:fn_ref, name, arity, _type}) do
    {:&, [], [{:/, [], [{name, [], nil}, arity]}]}
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

  # Raw tuple expression → Elixir tuple
  # {:tuple, elements, type} → {elem1, elem2, ...}
  def to_elixir({:tuple, elements, _type}) do
    elixir_elements = Enum.map(elements, &to_elixir/1)
    {:{}, [], elixir_elements}
  end

  # Receive expression → Elixir receive
  # (receive [:inc x] [:dec y]) → receive do :inc -> x; :dec -> y end
  def to_elixir({:receive, clauses, _type}) do
    clause_asts = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_ast = emit_pattern(pattern)
      body_ast = to_elixir(body)
      {:->, [], [[pattern_ast], body_ast]}
    end)

    {:receive, [], [[do: clause_asts]]}
  end

  # Do block → sequence of expressions, returning the last
  # (do expr1 expr2 expr3) → (expr1; expr2; expr3)
  def to_elixir({:do, exprs, _type}) do
    expr_asts = Enum.map(exprs, &to_elixir/1)
    {:__block__, [], expr_asts}
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

  # --- List operations ---

  # head: get first element of list
  def to_elixir({:call, :head, [list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    quote do: hd(unquote(list_ast))
  end

  # tail: get all but first element
  def to_elixir({:call, :tail, [list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    quote do: tl(unquote(list_ast))
  end

  # cons: prepend element to list
  def to_elixir({:call, :cons, [elem_expr, list_expr], _type}) do
    elem_ast = to_elixir(elem_expr)
    list_ast = to_elixir(list_expr)
    quote do: [unquote(elem_ast) | unquote(list_ast)]
  end

  # empty?: check if list is empty
  def to_elixir({:call, :empty?, [list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    quote do: unquote(list_ast) == []
  end

  # length: get list length
  def to_elixir({:call, :length, [list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    quote do: length(unquote(list_ast))
  end

  # str: convert args to strings and concatenate
  # Uses Kernel.to_string/1 for conversion and Enum.join for concatenation
  def to_elixir({:call, :str, args, _type}) do
    arg_asts = Enum.map(args, &to_elixir/1)
    # Build: Enum.map_join([arg1, arg2, ...], "", &to_string/1)
    # Or simpler: to_string(arg1) <> to_string(arg2) <> ...
    # Using Enum.join for cleaner generated code
    quote do
      Enum.map_join(unquote(arg_asts), "", &Kernel.to_string/1)
    end
  end

  # --- Anonymous functions ---

  # Anonymous function: (fn [x] body) → Elixir fn
  # Params can be atoms (:x) or typed vars ({:var, :x, :any})
  def to_elixir({:fn, params, body, _type}) do
    param_vars = Enum.map(params, fn
      {:var, name, _type} -> Macro.var(name, nil)
      name when is_atom(name) -> Macro.var(name, nil)
    end)
    body_ast = to_elixir(body)

    {:fn, [],
      [{:->, [], [param_vars, body_ast]}]}
  end

  # --- Higher-order list functions ---

  # map: apply function to each element (named function)
  def to_elixir({:call, :map, [func_name, list_expr], _type}) when is_atom(func_name) do
    list_ast = to_elixir(list_expr)
    # Build: Enum.map(list, fn x -> func_name(x) end)
    x_var = Macro.var(:vaisto_map_x, nil)
    call_ast = {func_name, [], [x_var]}
    quote do
      Enum.map(unquote(list_ast), fn unquote(x_var) -> unquote(call_ast) end)
    end
  end

  # map with anonymous function
  def to_elixir({:call, :map, [{:fn, _, _, _} = fn_ast, list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    fn_elixir = to_elixir(fn_ast)
    quote do
      Enum.map(unquote(list_ast), unquote(fn_elixir))
    end
  end

  # filter: keep elements where predicate is true (named function)
  def to_elixir({:call, :filter, [func_name, list_expr], _type}) when is_atom(func_name) do
    list_ast = to_elixir(list_expr)
    # Build: Enum.filter(list, fn x -> predicate(x) end)
    x_var = Macro.var(:vaisto_filter_x, nil)
    call_ast = {func_name, [], [x_var]}
    quote do
      Enum.filter(unquote(list_ast), fn unquote(x_var) -> unquote(call_ast) end)
    end
  end

  # filter with anonymous function
  def to_elixir({:call, :filter, [{:fn, _, _, _} = fn_ast, list_expr], _type}) do
    list_ast = to_elixir(list_expr)
    fn_elixir = to_elixir(fn_ast)
    quote do
      Enum.filter(unquote(list_ast), unquote(fn_elixir))
    end
  end

  # fold: left fold with accumulator (named function)
  def to_elixir({:call, :fold, [func_name, init_expr, list_expr], _type}) when is_atom(func_name) do
    init_ast = to_elixir(init_expr)
    list_ast = to_elixir(list_expr)
    # Build: Enum.reduce(list, init, fn elem, acc -> func(acc, elem) end)
    elem_var = Macro.var(:vaisto_fold_elem, nil)
    acc_var = Macro.var(:vaisto_fold_acc, nil)
    call_ast = {func_name, [], [acc_var, elem_var]}
    quote do
      Enum.reduce(unquote(list_ast), unquote(init_ast), fn unquote(elem_var), unquote(acc_var) -> unquote(call_ast) end)
    end
  end

  # fold with anonymous function
  # Note: anonymous fn params are (acc, elem) - need to swap for Enum.reduce which uses (elem, acc)
  def to_elixir({:call, :fold, [{:fn, params, body, fn_type}, init_expr, list_expr], _type}) do
    init_ast = to_elixir(init_expr)
    list_ast = to_elixir(list_expr)
    # User writes (fn [acc elem] body) but Enum.reduce calls with (elem, acc)
    # So we swap the params in the wrapper
    [acc_param, elem_param] = params
    swapped_fn = {:fn, [elem_param, acc_param], body, fn_type}
    fn_elixir = to_elixir(swapped_fn)
    quote do
      Enum.reduce(unquote(list_ast), unquote(init_ast), unquote(fn_elixir))
    end
  end

  # spawn: start a GenServer and return its PID
  # (spawn counter 0) → Counter.start_link(0) |> elem(1)
  def to_elixir({:call, :spawn, [process_name, init_arg], _pid_type}) do
    module = scoped_module_name(process_name)
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

  # unsafe send (!!): same runtime behavior as !, just different type-checking
  # The distinction is purely compile-time; at runtime they're identical.
  def to_elixir({:call, :"!!", [pid_expr, msg_expr], _type}) do
    pid = to_elixir(pid_expr)
    msg = to_elixir(msg_expr)

    quote do
      GenServer.call(unquote(pid), unquote(msg))
    end
  end

  # Type definitions - no runtime code needed, just documentation
  # Product types (records) and sum types (ADTs) are compile-time constructs
  def to_elixir({:deftype, _name, {:product, _fields}, _type}) do
    nil
  end
  def to_elixir({:deftype, _name, {:sum, _variants}, _type}) do
    nil
  end
  # Legacy format
  def to_elixir({:deftype, _name, _fields, _type}) do
    nil
  end

  # Variant construction → tagged tuple {:CtorName, field1, field2, ...}
  # Only when the call name is an actual constructor (variant) of the sum type
  def to_elixir({:call, ctor_name, args, {:sum, _sum_name, variants}}) do
    # Check if ctor_name is actually a variant constructor
    if List.keymember?(variants, ctor_name, 0) do
      typed_args = Enum.map(args, &to_elixir/1)
      {:{}, [], [ctor_name | typed_args]}
    else
      # Regular function call that happens to return a sum type
      {ctor_name, [], Enum.map(args, &to_elixir/1)}
    end
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

  # Multi-clause function definition → multiple Elixir def clauses
  # (defn len [[] 0] [[h | t] (+ 1 (len t))]) → def len([]), do: 0; def len([h | t]), do: 1 + len(t)
  # Note: typed AST has arity inserted: {:defn_multi, name, arity, clauses, type}
  def to_elixir({:defn_multi, name, _arity, clauses, _type}) do
    # Generate multiple def clauses, one per pattern
    clause_defs = Enum.map(clauses, fn {pattern, body, _body_type} ->
      pattern_ast = emit_fn_pattern(pattern)
      body_ast = to_elixir(body)

      quote do
        def unquote(name)(unquote(pattern_ast)) do
          unquote(body_ast)
        end
      end
    end)

    # Return a block of all defs
    {:__block__, [], clause_defs}
  end

  # Record construction → tagged tuple {:record_name, field1, field2, ...}
  def to_elixir({:call, name, args, {:record, name, _fields}}) do
    typed_args = Enum.map(args, &to_elixir/1)
    {:{}, [], [name | typed_args]}
  end

  # Qualified call: (erlang:hd xs) → :erlang.hd(xs)
  def to_elixir({:call, {:qualified, mod, func}, args, _type}) do
    typed_args = Enum.map(args, &to_elixir/1)
    {{:., [], [mod, func]}, [], typed_args}
  end

  # Extern declaration - no runtime code, just type information
  def to_elixir({:extern, _mod, _func, _func_type}) do
    nil
  end

  # Module system declarations - compile-time only, no runtime code
  def to_elixir({:ns, _name}) do
    nil
  end

  def to_elixir({:import, _module, _alias}) do
    nil
  end

  # Apply: calling a function stored in a variable (f.(args) syntax)
  def to_elixir({:apply, func_var, args, _type}) do
    func_ast = to_elixir(func_var)
    args_ast = Enum.map(args, &to_elixir/1)
    # In Elixir, calling a fn variable uses: f.(arg1, arg2)
    # AST: {{:., [], [f]}, [], [arg1, arg2]}
    {{:., [], [func_ast]}, [], args_ast}
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

  Returns `{:ok, module_name, bytecode}` or `{:error, %Vaisto.Error{}}`.
  For modules: returns `{:ok, :module, [{module_name, bytecode}, ...]}`.
  """
  @spec compile(TypeChecker.typed_ast(), atom()) ::
          {:ok, atom(), binary() | [{atom(), binary()}]} | {:error, Error.t()}
  def compile(typed_ast, module_name \\ :vaisto_module)

  # Module compilation - produces multiple modules (processes, supervisors)
  # or a single module with user-defined functions + main
  def compile({:module, _forms} = module_ast, module_name) do
    Process.put(:vaisto_compile_context, %{parent_module: module_name})

    try do
      compile_module(module_ast, module_name)
    after
      Process.delete(:vaisto_compile_context)
    end
  end

  # Process definition - compile directly to GenServer module
  def compile({:process, name, _init, _handlers, _type} = process_ast, module_name) do
    Process.put(:vaisto_compile_context, %{parent_module: module_name})

    try do
      elixir_ast = to_elixir(process_ast)
      module = scoped_module_name(name)

      try do
        [{^module, bytecode}] = Code.compile_quoted(elixir_ast)
        {:ok, module, bytecode}
      rescue
        e -> {:error, Error.new("compilation error", note: Exception.message(e))}
      end
    after
      Process.delete(:vaisto_compile_context)
    end
  end

  # Single defn compilation - place function in module with main as entry point
  def compile({:defn, name, _params, _body, _type} = defn_ast, module_name) do
    elixir_ast = to_elixir(defn_ast)

    # If the function is named "main", use it directly. Otherwise add a main that calls it.
    module_ast = if name == :main do
      quote do
        defmodule unquote(module_name) do
          unquote(elixir_ast)
        end
      end
    else
      quote do
        defmodule unquote(module_name) do
          unquote(elixir_ast)
          def main, do: unquote(name)()
        end
      end
    end

    try do
      [{^module_name, bytecode}] = Code.compile_quoted(module_ast)
      {:ok, module_name, bytecode}
    rescue
      e -> {:error, Error.new("compilation error", note: Exception.message(e))}
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
      e -> {:error, Error.new("compilation error", note: Exception.message(e))}
    end
  end

  # Private helpers

  defp compile_module(module_ast, module_name) do
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
          # De-duplicate function definitions BEFORE flattening
          # This preserves multi-clause functions (from defn_multi) as units
          # while allowing later single-clause defn to shadow earlier ones
          items = dedup_def_items(items)

          # Flatten any __block__ wrappers from defn_multi
          flattened = Enum.flat_map(items, fn
            {:__block__, [], block_items} -> block_items
            other -> [other]
          end)

          # Separate defn-generated defs from expression results
          {defs, exprs} = Enum.split_with(flattened, fn
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

          mod_ast = quote do
            defmodule unquote(module_name) do
              unquote_splicing(defs ++ main_def)
            end
          end

          Code.compile_quoted(mod_ast)
      end

      {:ok, module_name, main_results ++ standalone_results}
    rescue
      e -> {:error, Error.new("compilation error", note: Exception.message(e))}
    end
  end

  # De-duplicate function definition items by name, keeping the last definition
  # This implements shadowing semantics for REPL-style redefinition
  # Works on pre-flattened items to preserve multi-clause functions as units
  defp dedup_def_items(items) do
    items
    |> Enum.reverse()  # Process in reverse to keep last occurrence
    |> Enum.reduce({[], MapSet.new()}, fn item, {acc, seen} ->
      case extract_item_def_name(item) do
        nil ->
          # Not a def item (expression), always keep
          {[item | acc], seen}
        name ->
          if MapSet.member?(seen, name) do
            {acc, seen}  # Skip - already have a later definition
          else
            {[item | acc], MapSet.put(seen, name)}
          end
      end
    end)
    |> elem(0)
  end

  # Extract function name from a definition item (def or __block__ of defs)
  defp extract_item_def_name({:__block__, [], [{:def, _, _} | _] = defs}) do
    # Multi-clause function - get name from first clause
    extract_def_name(hd(defs))
  end
  defp extract_item_def_name({:def, _, _} = def_ast) do
    extract_def_name(def_ast)
  end
  defp extract_item_def_name(_), do: nil

  # Extract function name from def AST
  defp extract_def_name({:def, _, [{:when, _, [{name, _, _} | _]} | _]}), do: name
  defp extract_def_name({:def, _, [{name, _, _} | _]}), do: name

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
    module_name = scoped_module_name(name)
    handle_clauses = Enum.map(handlers, &emit_handle_call/1)

    quote do
      defmodule unquote(module_name) do
        use GenServer

        def start_link(init_arg) do
          GenServer.start_link(__MODULE__, init_arg)
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
  defp emit_pattern({:lit, :int, n}), do: n
  defp emit_pattern({:atom, a}), do: a  # Wrapped atom from parser

  # Empty list pattern: [] → []
  defp emit_pattern({:list_pattern, [], _type}), do: []
  defp emit_pattern({:list, [], _type}), do: []

  # Cons pattern: [h | t] → [h | t]
  defp emit_pattern({:cons_pattern, head, tail, _type}) do
    head_ast = emit_pattern(head)
    tail_ast = emit_pattern(tail)
    [{:|, [], [head_ast, tail_ast]}]
  end
  defp emit_pattern({:cons, head, tail, _type}) do
    head_ast = emit_pattern(head)
    tail_ast = emit_pattern(tail)
    [{:|, [], [head_ast, tail_ast]}]
  end

  # Tuple pattern: {a, b, c} → {a, b, c}
  defp emit_pattern({:tuple_pattern, elements, _type}) do
    pattern_elements = Enum.map(elements, &emit_pattern/1)
    {:{}, [], pattern_elements}
  end

  # Underscore is a wildcard pattern, not a literal atom
  defp emit_pattern(:_), do: Macro.var(:_, nil)
  defp emit_pattern(a) when is_atom(a), do: a
  defp emit_pattern(n) when is_integer(n), do: n

  # Function head patterns (for multi-clause functions)
  # These patterns are used directly as function arguments

  # Empty list pattern: [] → []
  defp emit_fn_pattern({:list, [], _type}), do: []

  # List with cons pattern: [h | t] → [h | t]
  defp emit_fn_pattern({:list, elements, _type}) do
    Enum.map(elements, &emit_fn_pattern/1)
  end

  # Cons pattern from typed AST: {:cons, head, tail}
  defp emit_fn_pattern({:cons, head, tail, _type}) do
    head_ast = emit_fn_pattern(head)
    tail_ast = emit_fn_pattern(tail)
    [{:|, [], [head_ast, tail_ast]}]
  end

  # Variable in pattern
  defp emit_fn_pattern({:var, name, _type}) do
    Macro.var(name, nil)
  end

  # Literals in patterns
  defp emit_fn_pattern({:lit, :int, n}), do: n
  defp emit_fn_pattern({:lit, :atom, a}), do: a
  defp emit_fn_pattern({:lit, :bool, b}), do: b

  # Untyped atoms (underscore, etc)
  defp emit_fn_pattern(:_), do: Macro.var(:_, nil)
  defp emit_fn_pattern(a) when is_atom(a), do: Macro.var(a, nil)
  defp emit_fn_pattern(n) when is_integer(n), do: n

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
    module = scoped_module_name(name)
    init_arg = case args do
      [arg] -> to_elixir(arg)
      args -> Enum.map(args, &to_elixir/1)
    end
    {module, init_arg}
  end

  defp child_spec_tuple({:call, name, args}) do
    module = scoped_module_name(name)
    init_arg = case args do
      [arg] -> to_elixir(arg)
      args -> Enum.map(args, &to_elixir/1)
    end
    {module, init_arg}
  end


  defp scoped_module_name(name) do
    base_name = Vaisto.Backend.Shared.camelize(name)

    case Process.get(:vaisto_compile_context) do
      %{parent_module: parent} when not is_nil(parent) ->
        Module.concat(parent, base_name)

      _ ->
        base_name
    end
  end

end
