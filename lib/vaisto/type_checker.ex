defmodule Vaisto.TypeChecker do
  @moduledoc """
  Type checker for Vaisto using Hindley-Milner style inference.
  
  Ensures type safety at compile time:
    (+ 1 :atom) → TypeError
    (+ 1 2)     → :int
  """

  # Built-in type environment
  # Note: spawn and send (!) are handled specially for typed PIDs
  # Note: head, tail, cons, map, filter, fold are handled specially for list types
  @primitives %{
    :+ => {:fn, [:int, :int], :int},
    :- => {:fn, [:int, :int], :int},
    :* => {:fn, [:int, :int], :int},
    :/ => {:fn, [:int, :int], :int},
    :== => {:fn, [:any, :any], :bool},
    :< => {:fn, [:int, :int], :bool},
    :> => {:fn, [:int, :int], :bool},
    :<= => {:fn, [:int, :int], :bool},
    :>= => {:fn, [:int, :int], :bool},
    :!= => {:fn, [:any, :any], :bool}
  }

  @doc """
  Check types and return the result type. Raises on error.
  """
  def check!(ast, env \\ @primitives) do
    case check(ast, env) do
      {:ok, _type, typed_ast} -> typed_ast
      {:error, msg} -> raise "TypeError: #{msg}"
    end
  end

  @doc """
  Check types and return {:ok, type, typed_ast} or {:error, reason}.
  """
  def check(ast, env \\ @primitives)

  # Module: list of top-level forms (process, supervise, def)
  def check(forms, env) when is_list(forms) do
    check_module(forms, env, [])
  end

  # Literals
  def check(n, _env) when is_integer(n), do: {:ok, :int, {:lit, :int, n}}
  def check(f, _env) when is_float(f), do: {:ok, :float, {:lit, :float, f}}
  def check(true, _env), do: {:ok, :bool, {:lit, :bool, true}}
  def check(false, _env), do: {:ok, :bool, {:lit, :bool, false}}
  def check({:string, s}, _env), do: {:ok, :string, {:lit, :string, s}}

  # List literal: (list 1 2 3) → homogeneous list
  def check({:list, []}, _env), do: {:ok, {:list, :any}, {:list, [], {:list, :any}}}
  def check({:list, elements}, env) do
    case check_args(elements, env) do
      {:ok, types, typed_elements} ->
        # Infer element type from first element (lists are homogeneous)
        elem_type = hd(types)
        # Verify all elements have the same type
        case Enum.find(types, &(not types_match?(elem_type, &1))) do
          nil ->
            list_type = {:list, elem_type}
            {:ok, list_type, {:list, typed_elements, list_type}}
          mismatched ->
            {:error, "List elements must have the same type: expected #{inspect(elem_type)}, got #{inspect(mismatched)}"}
        end
      error -> error
    end
  end

  # Atoms - could be message types OR variable references
  # If it's in the env (like :state in a handler), it's a variable
  def check(a, env) when is_atom(a) do
    case Map.get(env, a) do
      nil -> {:ok, {:atom, a}, {:lit, :atom, a}}
      type -> {:ok, type, {:var, a, type}}
    end
  end

  # Variable lookup
  def check({:var, name}, env) do
    case Map.get(env, name) do
      nil -> {:error, "Undefined variable: #{name}"}
      type -> {:ok, type, {:var, name, type}}
    end
  end

  # Special form: spawn - returns a typed PID
  # (spawn process_name initial_state) → Pid<ProcessName>
  def check({:call, :spawn, [process_name, init_state]}, env) do
    with {:ok, process_type} <- lookup_process(process_name, env),
         {:ok, _init_type, typed_init} <- check(init_state, env) do
      # Create typed PID that knows what messages this process accepts
      {:process, _state_type, accepted_msgs} = process_type
      pid_type = {:pid, process_name, accepted_msgs}
      {:ok, pid_type, {:call, :spawn, [process_name, typed_init], pid_type}}
    end
  end

  # --- List operations ---

  # head: (list a) → a
  def check({:call, :head, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, elem_type} ->
          {:ok, elem_type, {:call, :head, [typed_list], elem_type}}
        :any ->
          {:ok, :any, {:call, :head, [typed_list], :any}}
        other ->
          {:error, "head expects a list, got #{inspect(other)}"}
      end
    end
  end

  # tail: (list a) → (list a)
  def check({:call, :tail, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _elem_type} = t ->
          {:ok, t, {:call, :tail, [typed_list], t}}
        :any ->
          {:ok, {:list, :any}, {:call, :tail, [typed_list], {:list, :any}}}
        other ->
          {:error, "tail expects a list, got #{inspect(other)}"}
      end
    end
  end

  # cons: a → (list a) → (list a)
  def check({:call, :cons, [elem_expr, list_expr]}, env) do
    with {:ok, elem_type, typed_elem} <- check(elem_expr, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, :any} ->
          # Empty list - element determines type
          result_type = {:list, elem_type}
          {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}}
        {:list, list_elem_type} ->
          if types_match?(elem_type, list_elem_type) do
            result_type = {:list, list_elem_type}
            {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}}
          else
            {:error, "cons type mismatch: element is #{inspect(elem_type)}, list is #{inspect(list_type)}"}
          end
        :any ->
          result_type = {:list, elem_type}
          {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}}
        other ->
          {:error, "cons expects a list as second argument, got #{inspect(other)}"}
      end
    end
  end

  # empty?: (list a) → bool
  def check({:call, :empty?, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _} ->
          {:ok, :bool, {:call, :empty?, [typed_list], :bool}}
        :any ->
          {:ok, :bool, {:call, :empty?, [typed_list], :bool}}
        other ->
          {:error, "empty? expects a list, got #{inspect(other)}"}
      end
    end
  end

  # length: (list a) → int
  def check({:call, :length, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _} ->
          {:ok, :int, {:call, :length, [typed_list], :int}}
        :any ->
          {:ok, :int, {:call, :length, [typed_list], :int}}
        other ->
          {:error, "length expects a list, got #{inspect(other)}"}
      end
    end
  end

  # --- Higher-order list functions ---

  # map: (a → b) → (list a) → (list b)
  # (map func list) - applies func to each element
  def check({:call, :map, [func_name, list_expr]}, env) when is_atom(func_name) do
    with {:ok, func_type} <- lookup_function(func_name, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], ret_type}, {:list, _elem_type}} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [func_name, typed_list], result_type}}
        {{:fn, [_], ret_type}, :any} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [func_name, typed_list], result_type}}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, "map function must take exactly 1 argument, got #{length(args)}"}
        {_, {:list, _}} ->
          {:error, "map expects a function and a list"}
        {_, :any} ->
          {:error, "map expects a function and a list"}
        {_, other} ->
          {:error, "map expects a list as second argument, got #{inspect(other)}"}
      end
    end
  end

  # filter: (a → bool) → (list a) → (list a)
  # (filter predicate list) - keeps elements where predicate returns true
  def check({:call, :filter, [func_name, list_expr]}, env) when is_atom(func_name) do
    with {:ok, func_type} <- lookup_function(func_name, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], :bool}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [func_name, typed_list], {:list, elem_type}}}
        {{:fn, [_arg_type], :any}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [func_name, typed_list], {:list, elem_type}}}
        {{:fn, [_], :bool}, :any} ->
          {:ok, {:list, :any}, {:call, :filter, [func_name, typed_list], {:list, :any}}}
        {{:fn, [_], ret_type}, _} when ret_type not in [:bool, :any] ->
          {:error, "filter predicate must return bool, got #{inspect(ret_type)}"}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, "filter predicate must take exactly 1 argument, got #{length(args)}"}
        {_, {:list, _}} ->
          {:error, "filter expects a predicate function and a list"}
        {_, :any} ->
          {:error, "filter expects a predicate function and a list"}
        {_, other} ->
          {:error, "filter expects a list as second argument, got #{inspect(other)}"}
      end
    end
  end

  # fold: (b → a → b) → b → (list a) → b
  # (fold func init list) - left fold
  def check({:call, :fold, [func_name, init_expr, list_expr]}, env) when is_atom(func_name) do
    with {:ok, func_type} <- lookup_function(func_name, env),
         {:ok, init_type, typed_init} <- check(init_expr, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_acc_type, _elem_type], ret_type}, {:list, _}} ->
          {:ok, ret_type, {:call, :fold, [func_name, typed_init, typed_list], ret_type}}
        {{:fn, [_, _], ret_type}, :any} ->
          {:ok, ret_type, {:call, :fold, [func_name, typed_init, typed_list], ret_type}}
        {{:fn, args, _}, _} when length(args) != 2 ->
          {:error, "fold function must take exactly 2 arguments (acc, elem), got #{length(args)}"}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}}
        {_, other} ->
          {:error, "fold expects a list as third argument, got #{inspect(other)}"}
      end
    end
  end

  # Special form: send (!) - validates message against typed PID
  # (! pid message) → :ok, but only if message is valid for that PID
  def check({:call, :"!", [pid_expr, msg_expr]}, env) do
    with {:ok, pid_type, typed_pid} <- check(pid_expr, env),
         {:ok, msg_type, typed_msg} <- check(msg_expr, env) do
      case pid_type do
        {:pid, process_name, accepted_msgs} ->
          # Extract the message atom from the type
          msg_atom = case msg_type do
            {:atom, a} -> a
            _ -> nil
          end

          if msg_atom in accepted_msgs do
            {:ok, :ok, {:call, :"!", [typed_pid, typed_msg], :ok}}
          else
            {:error, "Process #{process_name} does not accept message :#{msg_atom}. " <>
                     "Valid messages: #{inspect(accepted_msgs)}"}
          end

        :pid ->
          # Untyped PID - allow any message (backward compat)
          {:ok, :ok, {:call, :"!", [typed_pid, typed_msg], :ok}}

        other ->
          {:error, "Expected a PID, got #{inspect(other)}"}
      end
    end
  end

  # If expression: (if cond then else)
  def check({:if, condition, then_branch, else_branch}, env) do
    with {:ok, cond_type, typed_cond} <- check(condition, env),
         :ok <- expect_bool(cond_type),
         {:ok, then_type, typed_then} <- check(then_branch, env),
         {:ok, else_type, typed_else} <- check(else_branch, env),
         :ok <- expect_same_type(then_type, else_type) do
      {:ok, then_type, {:if, typed_cond, typed_then, typed_else, then_type}}
    end
  end

  # Match expression: (match expr [pattern body] ...)
  def check({:match, expr, clauses}, env) do
    with {:ok, expr_type, typed_expr} <- check(expr, env),
         {:ok, result_type, typed_clauses} <- check_match_clauses(clauses, expr_type, env) do
      {:ok, result_type, {:match, typed_expr, typed_clauses, result_type}}
    end
  end

  # Let binding: (let [x 1 y 2] body)
  # Each binding extends the env for subsequent bindings and body
  def check({:let, bindings, body}, env) do
    case check_bindings(bindings, env, []) do
      {:ok, extended_env, typed_bindings} ->
        case check(body, extended_env) do
          {:ok, body_type, typed_body} ->
            {:ok, body_type, {:let, typed_bindings, typed_body, body_type}}
          error -> error
        end
      error -> error
    end
  end

  # Function call (general case)
  def check({:call, func, args}, env) do
    with {:ok, func_type} <- lookup_function(func, env),
         {:ok, arg_types, typed_args} <- check_args(args, env),
         {:ok, ret_type} <- unify_call(func_type, arg_types) do
      {:ok, ret_type, {:call, func, typed_args, ret_type}}
    end
  end

  # Process definition
  def check({:process, name, initial_state, handlers}, env) do
    with {:ok, state_type, _} <- check(initial_state, env),
         {:ok, typed_handlers} <- check_handlers(handlers, state_type, env) do
      process_type = {:process, state_type, handler_types(handlers)}
      {:ok, process_type, {:process, name, initial_state, typed_handlers, process_type}}
    end
  end

  # Supervision tree
  def check({:supervise, strategy, children}, env) do
    with :ok <- validate_strategy(strategy),
         {:ok, typed_children} <- check_children(children, env) do
      {:ok, :supervisor, {:supervise, strategy, typed_children}}
    end
  end

  # Type definition: (deftype point x y)
  # Registers a constructor function and a record type
  def check({:deftype, name, fields}, _env) do
    record_type = {:record, name, fields}
    {:ok, record_type, {:deftype, name, fields, record_type}}
  end

  # Function definition: (defn add [x y] (+ x y))
  # For now, parameters are typed as :any until we add type annotations
  def check({:defn, name, params, body}, env) do
    # Create env with parameters bound to :any
    param_types = Enum.map(params, fn _ -> :any end)
    param_env = Enum.zip(params, param_types) |> Map.new()

    # Add the function itself to env for recursion (with :any return type initially)
    self_type = {:fn, param_types, :any}
    extended_env = env |> Map.merge(param_env) |> Map.put(name, self_type)

    case check(body, extended_env) do
      {:ok, ret_type, typed_body} ->
        func_type = {:fn, param_types, ret_type}
        {:ok, func_type, {:defn, name, params, typed_body, func_type}}
      error -> error
    end
  end

  # Record construction: (point 1 2) when point is a record type
  # Handled in function call - lookup_function returns the constructor type

  # Fallback
  def check(other, _env) do
    {:error, "Unknown expression: #{inspect(other)}"}
  end

  # Helper functions

  defp lookup_function(name, env) do
    case Map.get(env, name) do
      nil -> {:error, "Unknown function: #{name}"}
      type -> {:ok, type}
    end
  end

  defp lookup_process(name, env) when is_atom(name) do
    case Map.get(env, name) do
      {:process, _, _} = process_type -> {:ok, process_type}
      nil -> {:error, "Unknown process: #{name}"}
      other -> {:error, "#{name} is not a process, got: #{inspect(other)}"}
    end
  end

  defp check_bindings([], env, acc) do
    {:ok, env, Enum.reverse(acc)}
  end

  defp check_bindings([{name, expr} | rest], env, acc) do
    case check(expr, env) do
      {:ok, type, typed_expr} ->
        extended_env = Map.put(env, name, type)
        check_bindings(rest, extended_env, [{name, typed_expr, type} | acc])
      error -> error
    end
  end

  # Check match clauses - each clause pattern extends env for its body
  defp check_match_clauses(clauses, expr_type, env) do
    results = Enum.map(clauses, fn {pattern, body} ->
      check_match_clause(pattern, body, expr_type, env)
    end)

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        typed_clauses = Enum.map(results, fn {:ok, clause} -> clause end)
        # All clauses must have the same result type - use the first one
        [{_pattern, _body, result_type} | _] = typed_clauses
        {:ok, result_type, typed_clauses}
    end
  end

  defp check_match_clause(pattern, body, expr_type, env) do
    # Extract bindings from pattern and add to env
    bindings = extract_pattern_bindings(pattern, expr_type, env)
    extended_env = Enum.reduce(bindings, env, fn {name, type}, acc ->
      Map.put(acc, name, type)
    end)

    # Type the pattern itself
    typed_pattern = type_pattern(pattern, expr_type, env)

    case check(body, extended_env) do
      {:ok, body_type, typed_body} ->
        {:ok, {typed_pattern, typed_body, body_type}}
      error -> error
    end
  end

  # Extract variable bindings from a pattern
  # (point x y) matching against {:record, :point, [:x, :y]} gives [{:x, :any}, {:y, :any}]
  defp extract_pattern_bindings({:call, record_name, args}, {:record, record_name, fields}, _env) do
    Enum.zip(args, fields)
    |> Enum.filter(fn {arg, _field} -> is_atom(arg) and arg not in [:_, true, false] end)
    |> Enum.map(fn {var_name, _field} -> {var_name, :any} end)
  end

  defp extract_pattern_bindings(var, _type, _env) when is_atom(var) and var not in [:_, true, false] do
    [{var, :any}]
  end

  defp extract_pattern_bindings(_, _, _), do: []

  # Type a pattern for the typed AST
  defp type_pattern({:call, record_name, args}, {:record, record_name, fields}, _env) do
    typed_args = Enum.map(args, fn
      var when is_atom(var) and var not in [:_, true, false] -> {:var, var, :any}
      {:call, _, _} = nested -> type_pattern(nested, :any, %{})
      lit -> lit
    end)
    {:pattern, record_name, typed_args, {:record, record_name, fields}}
  end

  defp type_pattern(var, type, _env) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, type}
  end

  defp type_pattern(lit, _type, _env) when is_integer(lit) or is_float(lit) or is_atom(lit) do
    lit
  end

  defp check_args(args, env) do
    results = Enum.map(args, &check(&1, env))
    
    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        types = Enum.map(results, fn {:ok, t, _} -> t end)
        typed = Enum.map(results, fn {:ok, _, ast} -> ast end)
        {:ok, types, typed}
    end
  end

  defp unify_call({:fn, expected_args, ret_type}, actual_args) do
    if length(expected_args) != length(actual_args) do
      {:error, "Arity mismatch: expected #{length(expected_args)}, got #{length(actual_args)}"}
    else
      mismatches = 
        Enum.zip(expected_args, actual_args)
        |> Enum.with_index()
        |> Enum.filter(fn {{exp, act}, _} -> not types_match?(exp, act) end)

      case mismatches do
        [] -> {:ok, ret_type}
        [{_, idx} | _] -> 
          {:error, "Type mismatch at argument #{idx + 1}"}
      end
    end
  end

  defp types_match?(:any, _), do: true
  defp types_match?(_, :any), do: true
  defp types_match?(t, t), do: true
  defp types_match?(_, _), do: false

  defp expect_bool(:bool), do: :ok
  defp expect_bool(:any), do: :ok
  defp expect_bool(other), do: {:error, "Expected bool in condition, got #{inspect(other)}"}

  defp expect_same_type(t, t), do: :ok
  defp expect_same_type(:any, _), do: :ok
  defp expect_same_type(_, :any), do: :ok
  defp expect_same_type(t1, t2), do: {:error, "Branch types must match: #{inspect(t1)} vs #{inspect(t2)}"}

  defp check_handlers(handlers, state_type, env) do
    handler_env = Map.put(env, :state, state_type)
    
    results = Enum.map(handlers, fn {msg, body} ->
      case check(body, handler_env) do
        {:ok, ret_type, typed_body} -> {:ok, {msg, typed_body, ret_type}}
        error -> error
      end
    end)

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil -> {:ok, Enum.map(results, fn {:ok, h} -> h end)}
    end
  end

  defp handler_types(handlers) do
    Enum.map(handlers, fn {msg, _} -> msg end)
  end

  defp validate_strategy(strategy) when strategy in [:one_for_one, :all_for_one, :rest_for_one] do
    :ok
  end
  defp validate_strategy(strategy), do: {:error, "Unknown supervision strategy: #{strategy}"}

  defp check_children(children, _env) do
    # For now, just validate they're well-formed
    {:ok, children}
  end

  # Check a module (list of top-level forms)
  # Two-pass approach for mutual recursion:
  # 1. First pass: register all function/type signatures
  # 2. Second pass: type-check all bodies with full environment
  defp check_module([], _env, acc) do
    {:ok, :module, {:module, Enum.reverse(acc)}}
  end

  defp check_module(forms, env, acc) when is_list(forms) and acc == [] do
    # First pass: collect all signatures
    env_with_signatures = Enum.reduce(forms, env, fn form, acc_env ->
      case form do
        {:defn, name, params, _body} ->
          param_types = Enum.map(params, fn _ -> :any end)
          func_type = {:fn, param_types, :any}
          Map.put(acc_env, name, func_type)

        {:deftype, name, fields} ->
          field_types = Enum.map(fields, fn _ -> :any end)
          record_type = {:record, name, fields}
          constructor_type = {:fn, field_types, record_type}
          Map.put(acc_env, name, constructor_type)

        {:process, name, initial_state, handlers} ->
          # Infer process type from handlers
          msg_types = Enum.map(handlers, fn {msg, _body} -> msg end)
          # Rough state type from initial_state
          state_type = case initial_state do
            n when is_integer(n) -> :int
            f when is_float(f) -> :float
            _ -> :any
          end
          process_type = {:process, state_type, msg_types}
          Map.put(acc_env, name, process_type)

        _ ->
          acc_env
      end
    end)

    # Second pass: type-check each form with full environment
    check_module_forms(forms, env_with_signatures, [])
  end

  defp check_module_forms([], _env, acc) do
    {:ok, :module, {:module, Enum.reverse(acc)}}
  end

  defp check_module_forms([form | rest], env, acc) do
    case check(form, env) do
      {:ok, _type, typed_form} ->
        # Update env with more precise types after checking
        new_env = case typed_form do
          {:process, name, _init, _handlers, process_type} ->
            Map.put(env, name, process_type)

          {:deftype, name, fields, record_type} ->
            field_types = Enum.map(fields, fn _ -> :any end)
            constructor_type = {:fn, field_types, record_type}
            Map.put(env, name, constructor_type)

          {:defn, name, _params, _body, func_type} ->
            Map.put(env, name, func_type)

          _ ->
            env
        end
        check_module_forms(rest, new_env, [typed_form | acc])

      {:error, _} = err ->
        err
    end
  end
end
