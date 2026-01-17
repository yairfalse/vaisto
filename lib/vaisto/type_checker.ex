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
  Return the built-in primitives type environment.
  """
  def primitives, do: @primitives

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
  Check types with source code for rich error formatting.
  Returns {:ok, type, typed_ast} or {:error, formatted_string}.

  The error string will be in Rust-style diagnostic format:

      error: type mismatch: expected Int, found Atom
        --> test.va:1:6
        |
      1 | (+ 1 :atom)
        |      ^^^^^
  """
  def check_with_source(ast, source, env \\ @primitives) do
    case check(ast, env) do
      {:ok, _, _} = success -> success
      {:error, msg} ->
        case Vaisto.ErrorFormatter.parse_legacy_error(msg) do
          nil ->
            # Can't parse, return as-is
            {:error, msg}
          error_map ->
            {:error, Vaisto.ErrorFormatter.format(error_map, source)}
        end
    end
  end

  @doc """
  Infer types using Hindley-Milner algorithm.

  This uses Algorithm W to infer types without explicit annotations.
  Supports let-polymorphism for generic functions.

  ## Examples

      iex> TypeChecker.infer({:fn, [:x], :x})
      {:ok, {:fn, [{:tvar, 0}], {:tvar, 0}}, _ast}  # identity: a -> a

      iex> TypeChecker.infer({:fn, [:x], {:call, :+, [:x, 1]}})
      {:ok, {:fn, [:int], :int}, _ast}  # inferred int -> int
  """
  def infer(ast, env \\ @primitives) do
    Vaisto.TypeSystem.Infer.infer(ast, env)
  end

  @doc """
  Check types and return {:ok, type, typed_ast} or {:error, reason}.
  """
  def check(ast, env \\ @primitives)

  # Module: list of top-level forms (process, supervise, def)
  def check(forms, env) when is_list(forms) do
    check_module(forms, env, [])
  end

  # Normalize AST nodes that have location metadata attached
  # Strip location from tuples to allow pattern matching on the core AST shape
  # Location is captured for error messages via with_loc/2
  def check({:call, func, args, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:call, func, args}, env), loc)
  def check({:if, cond, then_b, else_b, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:if, cond, then_b, else_b}, env), loc)
  def check({:let, bindings, body, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:let, bindings, body}, env), loc)
  def check({:match, expr, clauses, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:match, expr, clauses}, env), loc)
  def check({:receive, clauses, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:receive, clauses}, env), loc)
  def check({:process, name, init, handlers, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:process, name, init, handlers}, env), loc)
  def check({:supervise, strategy, children, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:supervise, strategy, children}, env), loc)
  def check({:def, name, args, body, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:def, name, args, body}, env), loc)
  def check({:defn, name, params, body, ret_type, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:defn, name, params, body, ret_type}, env), loc)
  # Legacy 4-arg defn (for backwards compatibility)
  def check({:defn, name, params, body, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:defn, name, params, body, :any}, env), loc)
  def check({:defn_multi, name, clauses, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:defn_multi, name, clauses}, env), loc)
  def check({:deftype, name, fields, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:deftype, name, fields}, env), loc)
  def check({:fn, params, body, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:fn, params, body}, env), loc)
  def check({:extern, mod, func, arg_types, ret_type, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:extern, mod, func, arg_types, ret_type}, env), loc)
  def check({:list, elements, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:list, elements}, env), loc)
  def check({:unit, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:unit}, env), loc)
  def check({:match_tuple, expr, clauses, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:match_tuple, expr, clauses}, env), loc)
  def check({:tuple_pattern, elements}, env), do: check_tuple_expr(elements, env)
  def check({:ns, name, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:ns, name}, env), loc)
  def check({:import, module, alias_name, %Vaisto.Parser.Loc{} = loc}, env), do: with_loc(check({:import, module, alias_name}, env), loc)

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

  # Atom literal from parser: {:atom, :foo} → :foo
  # This is always a literal, never a variable lookup
  def check({:atom, a}, _env) when is_atom(a) do
    {:ok, {:atom, a}, {:lit, :atom, a}}
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
  # Named function version
  # Skip built-in handling if user defined their own map function
  def check({:call, :map, [func_name, list_expr]}, env) when is_atom(func_name) do
    # If map is user-defined in env (not the built-in), use generic call handling
    case Map.get(env, :map) do
      nil -> check_builtin_map(func_name, list_expr, env)
      _user_defined -> check_generic_call(:map, [func_name, list_expr], env)
    end
  end

  defp check_builtin_map(func_name, list_expr, env) do
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

  # map with anonymous function (either 3-tuple or 4-tuple with location)
  def check({:call, :map, [{:fn, _, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :map) do
      nil -> check({:call, :map, [strip_fn_loc(fn_expr), list_expr]}, env)
      _user_defined -> check_generic_call(:map, [fn_expr, list_expr], env)
    end
  end
  def check({:call, :map, [{:fn, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :map) do
      nil -> check_builtin_map_anon(fn_expr, list_expr, env)
      _user_defined -> check_generic_call(:map, [fn_expr, list_expr], env)
    end
  end

  defp check_builtin_map_anon(fn_expr, list_expr, env) do
    with {:ok, func_type, typed_fn} <- check(fn_expr, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], ret_type}, {:list, _elem_type}} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [typed_fn, typed_list], result_type}}
        {{:fn, [_], ret_type}, :any} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [typed_fn, typed_list], result_type}}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, "map function must take exactly 1 argument, got #{length(args)}"}
        {_, other} ->
          {:error, "map expects a list as second argument, got #{inspect(other)}"}
      end
    end
  end

  # filter: (a → bool) → (list a) → (list a)
  # (filter predicate list) - keeps elements where predicate returns true
  # Named function version - skip built-in if user defined filter
  def check({:call, :filter, [func_name, list_expr]}, env) when is_atom(func_name) do
    case Map.get(env, :filter) do
      nil -> check_builtin_filter(func_name, list_expr, env)
      _user_defined -> check_generic_call(:filter, [func_name, list_expr], env)
    end
  end

  defp check_builtin_filter(func_name, list_expr, env) do
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

  # filter with anonymous function (either 3-tuple or 4-tuple with location)
  def check({:call, :filter, [{:fn, _, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :filter) do
      nil -> check({:call, :filter, [strip_fn_loc(fn_expr), list_expr]}, env)
      _user_defined -> check_generic_call(:filter, [fn_expr, list_expr], env)
    end
  end
  def check({:call, :filter, [{:fn, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :filter) do
      nil -> check_builtin_filter_anon(fn_expr, list_expr, env)
      _user_defined -> check_generic_call(:filter, [fn_expr, list_expr], env)
    end
  end

  defp check_builtin_filter_anon(fn_expr, list_expr, env) do
    with {:ok, func_type, typed_fn} <- check(fn_expr, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], :bool}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [typed_fn, typed_list], {:list, elem_type}}}
        {{:fn, [_arg_type], :any}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [typed_fn, typed_list], {:list, elem_type}}}
        {{:fn, [_], :bool}, :any} ->
          {:ok, {:list, :any}, {:call, :filter, [typed_fn, typed_list], {:list, :any}}}
        {{:fn, [_], ret_type}, _} when ret_type not in [:bool, :any] ->
          {:error, "filter predicate must return bool, got #{inspect(ret_type)}"}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, "filter predicate must take exactly 1 argument, got #{length(args)}"}
        {_, other} ->
          {:error, "filter expects a list as second argument, got #{inspect(other)}"}
      end
    end
  end

  # fold: (b → a → b) → b → (list a) → b
  # (fold func init list) - left fold
  # Named function version - skip built-in if user defined fold
  def check({:call, :fold, [func_name, init_expr, list_expr]}, env) when is_atom(func_name) do
    case Map.get(env, :fold) do
      nil -> check_builtin_fold(func_name, init_expr, list_expr, env)
      _user_defined -> check_generic_call(:fold, [func_name, init_expr, list_expr], env)
    end
  end

  defp check_builtin_fold(func_name, init_expr, list_expr, env) do
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

  # fold with anonymous function (either 3-tuple or 4-tuple with location)
  def check({:call, :fold, [{:fn, _, _, _} = fn_expr, init_expr, list_expr]}, env) do
    case Map.get(env, :fold) do
      nil -> check({:call, :fold, [strip_fn_loc(fn_expr), init_expr, list_expr]}, env)
      _user_defined -> check_generic_call(:fold, [fn_expr, init_expr, list_expr], env)
    end
  end
  def check({:call, :fold, [{:fn, _, _} = fn_expr, init_expr, list_expr]}, env) do
    case Map.get(env, :fold) do
      nil -> check_builtin_fold_anon(fn_expr, init_expr, list_expr, env)
      _user_defined -> check_generic_call(:fold, [fn_expr, init_expr, list_expr], env)
    end
  end

  defp check_builtin_fold_anon(fn_expr, init_expr, list_expr, env) do
    with {:ok, func_type, typed_fn} <- check(fn_expr, env),
         {:ok, init_type, typed_init} <- check(init_expr, env),
         {:ok, list_type, typed_list} <- check(list_expr, env) do
      case {func_type, list_type} do
        {{:fn, [_acc_type, _elem_type], ret_type}, {:list, _}} ->
          {:ok, ret_type, {:call, :fold, [typed_fn, typed_init, typed_list], ret_type}}
        {{:fn, [_, _], ret_type}, :any} ->
          {:ok, ret_type, {:call, :fold, [typed_fn, typed_init, typed_list], ret_type}}
        {{:fn, args, _}, _} when length(args) != 2 ->
          {:error, "fold function must take exactly 2 arguments (acc, elem), got #{length(args)}"}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}}
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

  # Match-tuple expression: (match-tuple expr [{:ok v} body] [{:error e} body2])
  # Used for interop with raw Erlang tuples - deliberately untyped
  def check({:match_tuple, expr, clauses}, env) do
    with {:ok, _expr_type, typed_expr} <- check(expr, env),
         {:ok, result_type, typed_clauses} <- check_match_tuple_clauses(clauses, env) do
      {:ok, result_type, {:match_tuple, typed_expr, typed_clauses, result_type}}
    end
  end

  # Receive expression: (receive [pattern body] ...)
  # Blocks until a message matching one of the patterns arrives
  # Patterns are typed as :any (full typed PIDs would constrain this)
  def check({:receive, clauses}, env) do
    with {:ok, result_type, typed_clauses} <- check_receive_clauses(clauses, env) do
      {:ok, result_type, {:receive, typed_clauses, result_type}}
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

  # Qualified call: (erlang:hd xs)
  # Must come before generic function call to match first
  def check({:call, {:qualified, mod, func}, args}, env) do
    # Look up the extern in environment using "mod:func" key
    extern_name = :"#{mod}:#{func}"
    case Map.get(env, extern_name) do
      nil ->
        {:error, "Unknown extern: #{mod}:#{func}"}
      {:fn, _param_types, ret_type} ->
        with {:ok, _arg_types, typed_args} <- check_args(args, env) do
          # Note: we're not enforcing arg type checking for externs yet
          {:ok, ret_type, {:call, {:qualified, mod, func}, typed_args, ret_type}}
        end
      other ->
        {:error, "#{mod}:#{func} is not a function, got: #{inspect(other)}"}
    end
  end

  # Helper for generic function call handling (used when user overrides built-ins)
  defp check_generic_call(func, args, env) do
    with {:ok, func_type} <- lookup_function(func, env),
         {:ok, arg_types, typed_args} <- check_args(args, env),
         {:ok, ret_type} <- unify_call(func_type, arg_types) do
      {:ok, ret_type, {:call, func, typed_args, ret_type}}
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

  # Product type (record): (deftype Point [x :int y :int])
  # Registers a constructor function and a record type
  def check({:deftype, name, {:product, fields}}, _env) do
    # Normalize field types
    normalized_fields = Enum.map(fields, fn {field_name, type} ->
      {field_name, parse_type_expr(type)}
    end)
    record_type = {:record, name, normalized_fields}
    {:ok, record_type, {:deftype, name, {:product, normalized_fields}, record_type}}
  end

  # Sum type (ADT): (deftype Result (Ok v) (Error msg))
  # Registers constructor functions for each variant
  # Each constructor returns the sum type
  def check({:deftype, name, {:sum, variants}}, _env) do
    # For now, treat type parameters as :any (we'll add full generics later)
    # Each variant becomes: CtorName => (field_types...) -> SumType
    normalized_variants = Enum.map(variants, fn {ctor_name, type_params} ->
      # Type params are just names for now - treat as :any
      field_types = Enum.map(type_params, fn _param -> :any end)
      {ctor_name, field_types}
    end)
    sum_type = {:sum, name, normalized_variants}
    {:ok, sum_type, {:deftype, name, {:sum, normalized_variants}, sum_type}}
  end

  # Legacy support: old-style deftype without wrapper
  def check({:deftype, name, fields}, env) when is_list(fields) do
    check({:deftype, name, {:product, fields}}, env)
  end

  # Function definition: (defn add [x :int y :int] (+ x y))
  # Params are now [{:x, :int}, {:y, :int}] tuples with types
  def check({:defn, name, params, body, declared_ret_type}, env) do
    # Extract param names and types
    param_names = Enum.map(params, fn {n, _t} -> n end)
    param_types = Enum.map(params, fn {_n, t} -> t end)
    param_env = Map.new(params)

    # Add the function itself to env for recursion
    self_type = {:fn, param_types, declared_ret_type}
    extended_env = env |> Map.merge(param_env) |> Map.put(name, self_type)

    case check(body, extended_env) do
      {:ok, inferred_ret_type, typed_body} ->
        # If return type was declared, verify it matches
        if declared_ret_type != :any and not types_match?(declared_ret_type, inferred_ret_type) do
          {:error, "Return type mismatch: declared #{inspect(declared_ret_type)}, but body has type #{inspect(inferred_ret_type)}"}
        else
          final_ret_type = if declared_ret_type != :any, do: declared_ret_type, else: inferred_ret_type
          func_type = {:fn, param_types, final_ret_type}
          {:ok, func_type, {:defn, name, param_names, typed_body, func_type}}
        end
      error -> error
    end
  end

  # Legacy form without return type annotation
  def check({:defn, name, params, body}, env) do
    check({:defn, name, params, body, :any}, env)
  end

  # Anonymous function: (fn [x] (* x 2))
  # Uses Hindley-Milner inference to determine parameter types
  def check({:fn, params, body}, env) do
    # Try inference first for better type information
    case Vaisto.TypeSystem.Infer.infer({:fn, params, body}, env) do
      {:ok, func_type, typed_ast} ->
        {:ok, func_type, typed_ast}

      {:error, _} ->
        # Fall back to :any for params if inference fails
        param_types = Enum.map(params, fn _ -> :any end)
        param_env = Enum.zip(params, param_types) |> Map.new()
        extended_env = Map.merge(env, param_env)

        case check(body, extended_env) do
          {:ok, ret_type, typed_body} ->
            func_type = {:fn, param_types, ret_type}
            {:ok, func_type, {:fn, params, typed_body, func_type}}
          error -> error
        end
    end
  end

  # Multi-clause function definition
  # (defn len [[] 0] [[h | t] (+ 1 (len t))])
  def check({:defn_multi, name, clauses}, env) do
    # Determine arity from first clause pattern
    # List patterns ([], [h|t], {:list, ...}) → arity 1 (single list argument)
    # Record patterns ({:call, name, args}) → arity = length(args)
    {first_pattern, _} = hd(clauses)
    arity = case first_pattern do
      [] -> 1  # Empty list pattern
      {:cons, _, _} -> 1  # Cons pattern [h | t]
      {:list, _} -> 1  # List literal pattern
      {:call, _, args} -> length(args)  # Record pattern
      _ -> 1
    end

    # Add function to env for recursion
    param_types = List.duplicate(:any, arity)
    self_type = {:fn, param_types, :any}
    extended_env = Map.put(env, name, self_type)

    # Type check each clause
    typed_clauses_result = Enum.reduce_while(clauses, {:ok, []}, fn {pattern, body}, {:ok, acc} ->
      # Extract bindings from pattern
      bindings = extract_multi_pattern_bindings(pattern)
      clause_env = Enum.reduce(bindings, extended_env, fn {var, type}, e ->
        Map.put(e, var, type)
      end)

      case check(body, clause_env) do
        {:ok, body_type, typed_body} ->
          typed_pattern = type_multi_pattern(pattern, env)
          {:cont, {:ok, [{typed_pattern, typed_body, body_type} | acc]}}
        error ->
          {:halt, error}
      end
    end)

    case typed_clauses_result do
      {:ok, typed_clauses} ->
        # Use first clause's return type (should unify, but simplified for now)
        [{_, _, ret_type} | _] = typed_clauses
        func_type = {:fn, param_types, ret_type}
        {:ok, func_type, {:defn_multi, name, arity, Enum.reverse(typed_clauses), func_type}}
      error -> error
    end
  end

  # Record construction: (point 1 2) when point is a record type
  # Handled in function call - lookup_function returns the constructor type

  # Extern declaration: (extern erlang:hd [:any] :any)
  # Registers the function signature in the environment
  def check({:extern, mod, func, arg_types, ret_type}, _env) do
    # Parse type expressions (e.g., {:call, :List, [:any]} → {:list, :any})
    parsed_arg_types = Enum.map(arg_types, &parse_type_expr/1)
    parsed_ret_type = parse_type_expr(ret_type)
    func_type = {:fn, parsed_arg_types, parsed_ret_type}
    {:ok, :extern, {:extern, mod, func, func_type}}
  end

  # Module declaration: (ns MyModule)
  # Compile-time only - sets the module name for the current file
  def check({:ns, name}, _env) do
    {:ok, :ns, {:ns, name}}
  end

  # Import declaration: (import Std.List) or (import Std.List :as L)
  # Compile-time only - brings another module's exports into scope
  # The actual loading of the interface is done by the build system
  def check({:import, module, alias_name}, _env) do
    {:ok, :import, {:import, module, alias_name}}
  end

  # Handle parse errors (propagate them as type errors with location info)
  def check({:error, msg, %Vaisto.Parser.Loc{} = loc}, _env) do
    file = if loc.file, do: "#{loc.file}:", else: ""
    {:error, "#{file}#{loc.line}:#{loc.col}: #{msg}"}
  end

  # Fallback
  def check(other, _env) do
    {:error, "Unknown expression: #{inspect(other)}"}
  end

  # Parse type expressions from extern declarations and type annotations
  # Atom-wrapped type from parser: {:atom, :int} → :int
  defp parse_type_expr({:atom, t}) when is_atom(t), do: t
  # Simple types: :int, :any, :string
  defp parse_type_expr(t) when is_atom(t), do: t
  # List type: {:call, :List, [:any]} → {:list, :any}
  defp parse_type_expr({:call, :List, [elem_type]}), do: {:list, parse_type_expr(elem_type)}
  # Fallback
  defp parse_type_expr(other), do: other

  # Strip location from fn AST nodes
  defp strip_fn_loc({:fn, params, body, %Vaisto.Parser.Loc{}}), do: {:fn, params, body}
  defp strip_fn_loc(other), do: other

  # Add location to error messages
  # Pass through success results, enhance error messages with line/column
  # Only add location if the error message doesn't already have one (starts with digit:)
  defp with_loc({:ok, _, _} = result, _loc), do: result
  defp with_loc({:error, msg}, %Vaisto.Parser.Loc{line: line, col: col, file: file}) do
    # Check if error already has location (format: "line:col:" or "file:line:col:")
    if String.match?(msg, ~r/^\d+:\d+:/) or String.match?(msg, ~r/^[^:]+:\d+:\d+:/) do
      # Error already has location, pass through
      {:error, msg}
    else
      prefix = case file do
        nil -> "#{line}:#{col}"
        f -> "#{f}:#{line}:#{col}"
      end
      {:error, "#{prefix}: #{msg}"}
    end
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
  # Also checks exhaustiveness for sum types
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

        # Exhaustiveness check for sum types
        case check_exhaustiveness(clauses, expr_type) do
          :ok -> {:ok, result_type, typed_clauses}
          {:error, _} = err -> err
        end
    end
  end

  # Check if pattern matching on sum types covers all variants
  defp check_exhaustiveness(clauses, {:sum, type_name, variants}) do
    # Extract constructor names from patterns
    covered = clauses
      |> Enum.map(fn {pattern, _body} -> extract_variant_name(pattern) end)
      |> Enum.reject(&is_nil/1)
      |> MapSet.new()

    # Check if there's a catch-all pattern (underscore or variable)
    has_catch_all = Enum.any?(clauses, fn {pattern, _body} ->
      is_catch_all_pattern?(pattern)
    end)

    if has_catch_all do
      :ok
    else
      all_variants = variants |> Enum.map(fn {name, _} -> name end) |> MapSet.new()
      missing = MapSet.difference(all_variants, covered)

      if MapSet.size(missing) == 0 do
        :ok
      else
        missing_list = missing |> MapSet.to_list() |> Enum.join(", ")
        {:error, "Non-exhaustive pattern match on #{type_name}. Missing variants: #{missing_list}"}
      end
    end
  end

  # Non-sum types don't need exhaustiveness checking (for now)
  defp check_exhaustiveness(_clauses, _expr_type), do: :ok

  # Extract variant constructor name from pattern
  defp extract_variant_name({:call, name, _, _}), do: name
  defp extract_variant_name({:pattern, name, _, _}), do: name
  defp extract_variant_name(_), do: nil

  # Check if pattern is a catch-all (matches anything)
  defp is_catch_all_pattern?(:_), do: true
  defp is_catch_all_pattern?(name) when is_atom(name) and name != :_, do: true
  defp is_catch_all_pattern?({:var, _, _}), do: true
  defp is_catch_all_pattern?(_), do: false

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

  # Check receive clauses - patterns receive messages typed as :any
  defp check_receive_clauses(clauses, env) do
    results = Enum.map(clauses, fn {pattern, body} ->
      check_receive_clause(pattern, body, env)
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

  defp check_receive_clause(pattern, body, env) do
    # Receive patterns match messages of type :any
    # Extract bindings from pattern - variables get :any type
    bindings = extract_pattern_bindings(pattern, :any, env)
    extended_env = Enum.reduce(bindings, env, fn {name, type}, acc ->
      Map.put(acc, name, type)
    end)

    # Type the pattern itself
    typed_pattern = type_pattern(pattern, :any, env)

    case check(body, extended_env) do
      {:ok, body_type, typed_body} ->
        {:ok, {typed_pattern, typed_body, body_type}}
      error -> error
    end
  end

  # Raw tuple expression: {:tuple_pattern, elements} when used as expression
  # Returns :any type since raw Erlang tuples are for interop with untyped code
  # This allows mixing different tuple shapes in if/match branches
  defp check_tuple_expr(elements, env) do
    case check_args(elements, env) do
      {:ok, _types, typed_elements} ->
        # Use :any for tuple type - these are for Erlang interop
        {:ok, :any, {:tuple, typed_elements, :any}}
      error -> error
    end
  end

  # Check match-tuple clauses - patterns are raw Erlang tuples, all bindings are :any
  defp check_match_tuple_clauses(clauses, env) do
    results = Enum.map(clauses, fn {pattern, body} ->
      check_match_tuple_clause(pattern, body, env)
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

  defp check_match_tuple_clause(pattern, body, env) do
    # Extract bindings from tuple pattern - all variables get :any type
    bindings = extract_tuple_pattern_bindings(pattern)
    extended_env = Enum.reduce(bindings, env, fn {name, type}, acc ->
      Map.put(acc, name, type)
    end)

    # Type the pattern itself
    typed_pattern = type_tuple_pattern(pattern)

    case check(body, extended_env) do
      {:ok, body_type, typed_body} ->
        {:ok, {typed_pattern, typed_body, body_type}}
      error -> error
    end
  end

  # Extract variable bindings from a raw tuple pattern
  # {:tuple_pattern, [{:atom, :ok}, :v]} → [{:v, :any}]
  defp extract_tuple_pattern_bindings({:tuple_pattern, elements}) do
    Enum.flat_map(elements, &extract_tuple_element_bindings/1)
  end
  defp extract_tuple_pattern_bindings(var) when is_atom(var) and var not in [:_, true, false] do
    # Catch-all variable pattern
    [{var, :any}]
  end
  defp extract_tuple_pattern_bindings(_), do: []

  defp extract_tuple_element_bindings(var) when is_atom(var) and var not in [:_, true, false] do
    [{var, :any}]
  end
  defp extract_tuple_element_bindings({:tuple_pattern, elements}) do
    Enum.flat_map(elements, &extract_tuple_element_bindings/1)
  end
  defp extract_tuple_element_bindings(_), do: []

  # Type a raw tuple pattern for the typed AST
  defp type_tuple_pattern({:tuple_pattern, elements}) do
    typed_elements = Enum.map(elements, &type_tuple_element/1)
    {:tuple_pattern, typed_elements, :any}
  end
  defp type_tuple_pattern(var) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, :any}
  end
  defp type_tuple_pattern(other), do: other

  defp type_tuple_element({:atom, a}), do: {:lit, :atom, a}
  defp type_tuple_element({:tuple_pattern, elements}) do
    typed_elements = Enum.map(elements, &type_tuple_element/1)
    {:tuple_pattern, typed_elements, :any}
  end
  defp type_tuple_element(var) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, :any}
  end
  defp type_tuple_element(lit) when is_integer(lit), do: {:lit, :int, lit}
  defp type_tuple_element(lit) when is_atom(lit), do: {:lit, :atom, lit}
  defp type_tuple_element(other), do: other

  # Extract variable bindings from a pattern with proper types
  # (point x y) matching against {:record, :point, [{:x, :int}, {:y, :int}]}
  # gives [{:x, :int}, {:y, :int}]
  # Normalize patterns with location
  defp extract_pattern_bindings({:call, name, args, %Vaisto.Parser.Loc{}}, type, env) do
    extract_pattern_bindings({:call, name, args}, type, env)
  end

  defp extract_pattern_bindings({:call, record_name, args}, {:record, record_name, fields}, _env) do
    Enum.zip(args, fields)
    |> Enum.filter(fn {arg, _field} -> is_atom(arg) and arg not in [:_, true, false] end)
    |> Enum.map(fn {var_name, {_field_name, field_type}} -> {var_name, field_type} end)
  end

  # Variant pattern against sum type
  defp extract_pattern_bindings({:call, ctor_name, args}, {:sum, _sum_name, variants}, _env) do
    case List.keyfind(variants, ctor_name, 0) do
      {^ctor_name, field_types} ->
        Enum.zip(args, field_types)
        |> Enum.filter(fn {arg, _} -> is_atom(arg) and arg not in [:_, true, false] end)
        |> Enum.map(fn {var_name, field_type} -> {var_name, field_type} end)
      nil ->
        []
    end
  end

  # Record pattern against :any type - try to look up the record/variant in env
  defp extract_pattern_bindings({:call, record_name, args}, :any, env) do
    case Map.get(env, record_name) do
      {:fn, _arg_types, {:record, ^record_name, fields}} ->
        # Found the constructor, use its field types
        extract_pattern_bindings({:call, record_name, args}, {:record, record_name, fields}, env)
      {:fn, _arg_types, {:sum, _sum_name, _variants} = sum_type} ->
        # Found a variant constructor, use the sum type
        extract_pattern_bindings({:call, record_name, args}, sum_type, env)
      _ ->
        # Can't find record type, fall back to :any for all vars
        args
        |> Enum.filter(fn arg -> is_atom(arg) and arg not in [:_, true, false] end)
        |> Enum.map(fn var_name -> {var_name, :any} end)
    end
  end

  defp extract_pattern_bindings(var, type, _env) when is_atom(var) and var not in [:_, true, false] do
    [{var, type}]
  end

  defp extract_pattern_bindings(_, _, _), do: []

  # Type a pattern for the typed AST
  # Normalize patterns with location metadata first
  defp type_pattern({:call, record_name, args, %Vaisto.Parser.Loc{}}, expected_type, env) do
    type_pattern({:call, record_name, args}, expected_type, env)
  end

  # Uses field types from the record definition
  defp type_pattern({:call, record_name, args}, {:record, record_name, fields}, _env) do
    typed_args = Enum.zip(args, fields)
    |> Enum.map(fn
      {var, {_field_name, field_type}} when is_atom(var) and var not in [:_, true, false] ->
        {:var, var, field_type}
      {{:call, _, _} = nested, _field} ->
        type_pattern(nested, :any, %{})
      {lit, _field} ->
        lit
    end)
    {:pattern, record_name, typed_args, {:record, record_name, fields}}
  end

  # Variant pattern against sum type: (Ok v) matched against Result
  defp type_pattern({:call, ctor_name, args}, {:sum, sum_name, variants}, _env) do
    # Find the variant in the sum type
    case List.keyfind(variants, ctor_name, 0) do
      {^ctor_name, field_types} ->
        # Type the args according to the variant's field types
        typed_args = Enum.zip(args, field_types)
        |> Enum.map(fn
          {var, field_type} when is_atom(var) and var not in [:_, true, false] ->
            {:var, var, field_type}
          {lit, _field_type} when is_integer(lit) or is_float(lit) ->
            lit
          {:_, _field_type} ->
            :_
        end)
        {:pattern, ctor_name, typed_args, {:sum, sum_name, variants}}
      nil ->
        # Unknown variant - this shouldn't happen if type checking is correct
        {:pattern, ctor_name, args, :any}
    end
  end

  # Record pattern against :any type - try to look up the record in env
  defp type_pattern({:call, record_name, args}, :any, env) do
    case Map.get(env, record_name) do
      {:fn, _arg_types, {:record, ^record_name, fields}} ->
        # Found the constructor, use its field types
        type_pattern({:call, record_name, args}, {:record, record_name, fields}, env)
      {:fn, _arg_types, {:sum, _sum_name, _variants} = sum_type} ->
        # Found a variant constructor, use the sum type
        type_pattern({:call, record_name, args}, sum_type, env)
      _ ->
        # Can't find record type, fall back to :any for all vars
        typed_args = Enum.map(args, fn
          var when is_atom(var) and var not in [:_, true, false] -> {:var, var, :any}
          {:call, _, _} = nested -> type_pattern(nested, :any, env)
          lit -> lit
        end)
        {:pattern, record_name, typed_args, :any}
    end
  end

  # Atom literal pattern: {:atom, :foo} → {:lit, :atom, :foo}
  defp type_pattern({:atom, a}, _type, _env) when is_atom(a) do
    {:lit, :atom, a}
  end

  defp type_pattern(var, type, _env) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, type}
  end

  defp type_pattern(lit, _type, _env) when is_integer(lit) or is_float(lit) or is_atom(lit) do
    lit
  end

  # --- Multi-clause function helpers ---

  # Extract bindings from patterns in multi-clause functions
  # Empty list pattern: []
  defp extract_multi_pattern_bindings([]), do: []
  # Non-empty list pattern from parser: [x] parses as [:x]
  defp extract_multi_pattern_bindings(elems) when is_list(elems) do
    Enum.flat_map(elems, &extract_multi_pattern_bindings/1)
  end
  defp extract_multi_pattern_bindings({:list, elements}) do
    Enum.flat_map(elements, &extract_multi_pattern_bindings/1)
  end
  # Cons pattern: {:cons, head, tail}
  defp extract_multi_pattern_bindings({:cons, head, tail}) do
    extract_multi_pattern_bindings(head) ++ extract_multi_pattern_bindings(tail)
  end
  defp extract_multi_pattern_bindings({:call, _name, args}) do
    Enum.flat_map(args, &extract_multi_pattern_bindings/1)
  end
  defp extract_multi_pattern_bindings(var) when is_atom(var) and var not in [:_, true, false] do
    [{var, :any}]
  end
  defp extract_multi_pattern_bindings(_), do: []

  # Type a pattern for multi-clause function
  # Empty list pattern: [] → {:list, [], {:list, :any}}
  defp type_multi_pattern([], _env) do
    {:list, [], {:list, :any}}
  end
  defp type_multi_pattern({:list, []}, _env) do
    {:list, [], {:list, :any}}
  end
  defp type_multi_pattern({:list, elements}, env) do
    typed_elements = Enum.map(elements, &type_multi_pattern(&1, env))
    {:list, typed_elements, {:list, :any}}
  end
  # List literal pattern from parser: [x] parses as [:x]
  defp type_multi_pattern(elems, env) when is_list(elems) and elems != [] do
    typed_elements = Enum.map(elems, &type_multi_pattern(&1, env))
    {:list, typed_elements, {:list, :any}}
  end
  # Cons pattern: {:cons, head, tail} → {:cons, typed_head, typed_tail, type}
  defp type_multi_pattern({:cons, head, tail}, env) do
    typed_head = type_multi_pattern(head, env)
    typed_tail = type_multi_pattern(tail, env)
    {:cons, typed_head, typed_tail, {:list, :any}}
  end
  defp type_multi_pattern({:call, name, args}, env) do
    typed_args = Enum.map(args, &type_multi_pattern(&1, env))
    {:pattern, name, typed_args, :any}
  end
  defp type_multi_pattern(var, _env) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, :any}
  end
  defp type_multi_pattern(lit, _env) when is_integer(lit), do: {:lit, :int, lit}
  defp type_multi_pattern(lit, _env) when is_atom(lit), do: {:lit, :atom, lit}

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

  # When the function type is :any (untyped higher-order function parameter)
  # allow any arguments and return :any
  defp unify_call(:any, _actual_args) do
    {:ok, :any}
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
    Enum.map(handlers, fn
      {{:atom, msg}, _} -> msg
      {msg, _} -> msg
    end)
  end

  defp validate_strategy({:atom, strategy}) when strategy in [:one_for_one, :all_for_one, :rest_for_one] do
    :ok
  end
  defp validate_strategy(strategy) when strategy in [:one_for_one, :all_for_one, :rest_for_one] do
    :ok
  end
  defp validate_strategy(strategy), do: {:error, "Unknown supervision strategy: #{inspect(strategy)}"}

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
    # Patterns handle both with and without location metadata
    env_with_signatures = Enum.reduce(forms, env, fn form, acc_env ->
      case form do
        # 6-tuple defn with return type and location
        {:defn, name, params, _body, ret_type, %Vaisto.Parser.Loc{}} ->
          collect_defn_signature(name, params, ret_type, acc_env)

        # 5-tuple defn with return type (no location)
        {:defn, name, params, _body, ret_type} when not is_struct(ret_type, Vaisto.Parser.Loc) ->
          collect_defn_signature(name, params, ret_type, acc_env)

        # Legacy 5-tuple defn without return type (has location)
        {:defn, name, params, _body, %Vaisto.Parser.Loc{}} ->
          collect_defn_signature(name, params, :any, acc_env)

        # Legacy 4-tuple defn without return type or location
        {:defn, name, params, _body} ->
          collect_defn_signature(name, params, :any, acc_env)

        {:defn_multi, name, clauses, %Vaisto.Parser.Loc{}} ->
          collect_defn_multi_signature(name, clauses, acc_env)

        {:defn_multi, name, clauses} ->
          collect_defn_multi_signature(name, clauses, acc_env)

        {:deftype, name, fields, %Vaisto.Parser.Loc{}} ->
          collect_deftype_signature(name, fields, acc_env)

        {:deftype, name, fields} ->
          collect_deftype_signature(name, fields, acc_env)

        {:process, name, initial_state, handlers, %Vaisto.Parser.Loc{}} ->
          collect_process_signature(name, initial_state, handlers, acc_env)

        {:process, name, initial_state, handlers} ->
          collect_process_signature(name, initial_state, handlers, acc_env)

        {:extern, mod, func, arg_types, ret_type, %Vaisto.Parser.Loc{}} ->
          collect_extern_signature(mod, func, arg_types, ret_type, acc_env)

        {:extern, mod, func, arg_types, ret_type} ->
          collect_extern_signature(mod, func, arg_types, ret_type, acc_env)

        _ ->
          acc_env
      end
    end)

    # Second pass: type-check each form with full environment
    check_module_forms(forms, env_with_signatures, [])
  end

  # Signature collection helpers
  defp collect_defn_signature(name, params, ret_type, env) do
    # Params are now [{:x, :int}, {:y, :int}] tuples
    param_types = Enum.map(params, fn {_name, type} -> type end)
    func_type = {:fn, param_types, ret_type}
    Map.put(env, name, func_type)
  end

  defp collect_defn_multi_signature(name, clauses, env) do
    # Determine arity from first clause
    {first_pattern, _} = hd(clauses)
    arity = case first_pattern do
      {:list, elems} -> length(elems)
      {:list, elems, _loc} -> length(elems)
      {:call, _, args} -> length(args)
      {:call, _, args, _loc} -> length(args)
      _ when is_list(first_pattern) -> length(first_pattern)
      _ -> 1
    end
    param_types = List.duplicate(:any, arity)
    func_type = {:fn, param_types, :any}
    Map.put(env, name, func_type)
  end

  defp collect_deftype_signature(name, {:product, fields}, env) do
    # Product type (record): single constructor
    normalized_fields = Enum.map(fields, fn {field_name, type} ->
      {field_name, parse_type_expr(type)}
    end)
    field_types = Enum.map(normalized_fields, fn {_name, type} -> type end)
    record_type = {:record, name, normalized_fields}
    constructor_type = {:fn, field_types, record_type}
    Map.put(env, name, constructor_type)
  end

  defp collect_deftype_signature(name, {:sum, variants}, env) do
    # Sum type: constructor for each variant
    normalized_variants = Enum.map(variants, fn {ctor_name, type_params} ->
      field_types = Enum.map(type_params, fn _param -> :any end)
      {ctor_name, field_types}
    end)
    sum_type = {:sum, name, normalized_variants}

    # Register sum type and all constructors
    env_with_type = Map.put(env, name, sum_type)
    Enum.reduce(normalized_variants, env_with_type, fn {ctor_name, field_types}, acc_env ->
      constructor_type = {:fn, field_types, sum_type}
      Map.put(acc_env, ctor_name, constructor_type)
    end)
  end

  # Legacy: list of fields without wrapper
  defp collect_deftype_signature(name, fields, env) when is_list(fields) do
    collect_deftype_signature(name, {:product, fields}, env)
  end

  defp collect_process_signature(name, initial_state, handlers, env) do
    # Infer process type from handlers
    # Unwrap {:atom, msg} if present
    msg_types = Enum.map(handlers, fn
      {{:atom, msg}, _body} -> msg
      {msg, _body} -> msg
    end)
    # Rough state type from initial_state
    state_type = case initial_state do
      n when is_integer(n) -> :int
      f when is_float(f) -> :float
      _ -> :any
    end
    process_type = {:process, state_type, msg_types}
    Map.put(env, name, process_type)
  end

  defp collect_extern_signature(mod, func, arg_types, ret_type, env) do
    # Register extern function under "mod:func" key
    extern_name = :"#{mod}:#{func}"
    parsed_arg_types = Enum.map(arg_types, &parse_type_expr/1)
    parsed_ret_type = parse_type_expr(ret_type)
    func_type = {:fn, parsed_arg_types, parsed_ret_type}
    Map.put(env, extern_name, func_type)
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

          {:deftype, name, {:product, fields}, record_type} ->
            # Product type: register single constructor
            field_types = Enum.map(fields, fn {_name, type} -> type end)
            constructor_type = {:fn, field_types, record_type}
            Map.put(env, name, constructor_type)

          {:deftype, name, {:sum, variants}, sum_type} ->
            # Sum type: register constructor for each variant
            # Also register the sum type itself for type annotations
            env_with_type = Map.put(env, name, sum_type)
            Enum.reduce(variants, env_with_type, fn {ctor_name, field_types}, acc_env ->
              constructor_type = {:fn, field_types, sum_type}
              Map.put(acc_env, ctor_name, constructor_type)
            end)

          {:defn, name, _params, _body, func_type} ->
            Map.put(env, name, func_type)

          {:defn_multi, name, _arity, _clauses, func_type} ->
            Map.put(env, name, func_type)

          {:extern, mod, func, func_type} ->
            # Extern already registered in first pass, but keep in typed forms
            extern_name = :"#{mod}:#{func}"
            Map.put(env, extern_name, func_type)

          _ ->
            env
        end
        check_module_forms(rest, new_env, [typed_form | acc])

      {:error, _} = err ->
        err
    end
  end
end
