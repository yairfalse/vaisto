defmodule Vaisto.TypeChecker do
  @moduledoc """
  Type checker for Vaisto using Hindley-Milner style inference.

  Ensures type safety at compile time:
    (+ 1 :atom) → TypeError
    (+ 1 2)     → :int
  """

  require Logger

  alias Vaisto.Error
  alias Vaisto.Errors
  alias Vaisto.TypeEnv

  @type ast :: term()

  @type vaisto_type ::
          :int
          | :float
          | :string
          | :bool
          | :atom
          | :unit
          | :any
          | :num
          | {:atom, atom()}
          | {:tvar, non_neg_integer()}
          | {:rvar, non_neg_integer()}
          | {:fn, [vaisto_type()], vaisto_type()}
          | {:list, vaisto_type()}
          | {:record, atom(), [{atom(), vaisto_type()}]}
          | {:sum, atom(), [{atom(), [vaisto_type()]}]}
          | {:row, [{atom(), vaisto_type()}], :closed | {:rvar, non_neg_integer()}}
          | {:pid, atom(), [atom()]}
          | {:process, vaisto_type(), [atom()]}

  @type typed_pattern ::
          {:pattern, atom(), [typed_ast()], vaisto_type()}
          | {:cons_pattern, typed_ast(), typed_ast(), vaisto_type()}
          | {:list_pattern, [typed_ast()], vaisto_type()}
          | {:tuple_pattern, [typed_ast()], vaisto_type()}
          | {:var, atom(), vaisto_type()}
          | {:lit, atom(), term()}
          | :_
          | atom()
          | integer()

  @type typed_clause :: {typed_pattern(), typed_ast(), vaisto_type()}

  @type typed_ast ::
          # Literals
          {:lit, atom(), term()}
          # Variables and references
          | {:var, atom(), vaisto_type()}
          | {:fn_ref, atom(), non_neg_integer(), vaisto_type()}
          # Collections
          | {:list, [typed_ast()], vaisto_type()}
          | {:cons, typed_ast(), typed_ast(), vaisto_type()}
          | {:tuple, [typed_ast()], vaisto_type()}
          | {:map, [{typed_ast(), typed_ast()}], vaisto_type()}
          # Control flow
          | {:if, typed_ast(), typed_ast(), typed_ast(), vaisto_type()}
          | {:match, typed_ast(), [typed_clause()], vaisto_type()}
          | {:receive, [typed_clause()], vaisto_type()}
          | {:do, [typed_ast()], vaisto_type()}
          # Bindings
          | {:let, [term()], typed_ast(), vaisto_type()}
          | {:fn, [typed_ast() | atom()], typed_ast(), vaisto_type()}
          # Calls
          | {:call, atom() | {:qualified, atom(), atom()}, [typed_ast()], vaisto_type()}
          | {:apply, typed_ast(), [typed_ast()], vaisto_type()}
          # Definitions
          | {:defn, atom(), [atom()], typed_ast(), vaisto_type()}
          | {:defn_multi, atom(), non_neg_integer(), [typed_clause()], vaisto_type()}
          | {:defval, atom(), typed_ast(), vaisto_type()}
          | {:deftype, atom(), {:product | :sum, term()}, vaisto_type()}
          # Process/concurrency
          | {:process, atom(), typed_ast(), [typed_clause()], vaisto_type()}
          | {:supervise, term(), [typed_ast()], vaisto_type()}
          # Field access
          | {:field_access, typed_ast(), atom(), vaisto_type()}
          | {:field_access, typed_ast(), atom(), vaisto_type(), vaisto_type()}
          # Declarations
          | {:extern, atom(), atom(), vaisto_type()}
          | {:ns, atom()}
          | {:import, atom(), atom() | nil}
          # Module container
          | {:module, [typed_ast()]}

  @type type_env :: map()

  # Use TypeEnv for built-in primitives
  # Note: spawn and send (!) are handled specially for typed PIDs
  # Note: head, tail, cons, map, filter, fold are handled specially for list types
  @primitives TypeEnv.primitives()

  @doc """
  Return the built-in primitives type environment.
  """
  @spec primitives() :: type_env()
  def primitives, do: TypeEnv.primitives()

  @doc """
  Check types and return the result type. Raises on error.
  """
  @spec check!(ast(), type_env()) :: typed_ast()
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

      error[E001]: type mismatch
        --> test.va:1:6
        |
      1 | (+ 1 :atom)
        |      ^^^^^ expected `Int`, found `Atom`
  """
  @spec check_with_source(ast(), String.t(), type_env()) ::
          {:ok, term(), typed_ast()} | {:error, String.t()}
  def check_with_source(ast, source, env \\ @primitives) do
    case check(ast, env) do
      {:ok, _, _} = success -> success
      {:error, errors} when is_list(errors) ->
        # Multiple errors - format all
        {:error, Vaisto.ErrorFormatter.format_all(errors, source)}
      {:error, %Error{} = error} ->
        # Structured error - format with rich display
        {:error, Vaisto.ErrorFormatter.format(error, source)}
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
  @spec infer(ast(), type_env()) :: {:ok, term(), typed_ast()} | {:error, term()}
  def infer(ast, env \\ @primitives) do
    Vaisto.TypeSystem.Infer.infer(ast, env)
  end

  @doc """
  Check types and return {:ok, type, typed_ast} or {:error, reason}.
  """
  @spec check(ast(), type_env()) ::
          {:ok, term(), typed_ast()} | {:error, Error.t() | [Error.t()]}
  def check(ast, env \\ @primitives)

  # Module: list of top-level forms (process, supervise, def)
  def check(forms, env) when is_list(forms) do
    check_module(forms, env, [])
  end

  # Generic location handling for all AST tuple nodes
  # Strips Loc from any tuple and delegates to check_impl, wrapping result with location
  # This replaces 27 individual boilerplate clauses
  def check(node, env) when is_tuple(node) do
    case Vaisto.LocationStripper.strip_node(node) do
      {^node, nil} ->
        # No Loc found, node unchanged - delegate to implementation
        check_impl(node, env)
      {stripped, %Vaisto.Parser.Loc{} = loc} ->
        # Loc found and stripped - check stripped version and wrap result
        with_loc(check(stripped, env), loc)
    end
  end

  # Literals (non-tuple primitives)
  def check(n, _env) when is_integer(n), do: {:ok, :int, {:lit, :int, n}}
  def check(f, _env) when is_float(f), do: {:ok, :float, {:lit, :float, f}}
  def check(true, _env), do: {:ok, :bool, {:lit, :bool, true}}
  def check(false, _env), do: {:ok, :bool, {:lit, :bool, false}}

  # Atoms - could be message types OR variable references
  # If it's in the env (like :state in a handler), it's a variable
  def check(a, env) when is_atom(a) do
    case Map.get(env, a) do
      nil ->
        {:ok, {:atom, a}, {:lit, :atom, a}}

      {:fn, params, _ret} = type ->
        # Function type - check if it's local or module-level
        if is_local_var?(a, env) do
          {:ok, type, {:var, a, type}}
        else
          # Module-level function - emit fn_ref so emitter can use &name/arity
          {:ok, type, {:fn_ref, a, length(params), type}}
        end

      type ->
        {:ok, type, {:var, a, type}}
    end
  end

  # Fallback for unrecognized expressions
  def check(other, _env) do
    {:error, Errors.unknown_expression(other)}
  end

  # ============================================================================
  # Private implementation clauses (check_impl) for tuple AST nodes
  # ============================================================================

  # Delegate tuple patterns and tuples to their handlers
  defp check_impl({:tuple_pattern, elements}, env), do: check_tuple_expr(elements, env)
  defp check_impl({:map, pairs}, env), do: check_map_literal(pairs, env)
  defp check_impl({:tuple, elements}, env), do: check_tuple_expr(elements, env)

  # Bracket expressions: [] or [a b c] used as list literals
  # Empty bracket [] → empty list
  defp check_impl({:bracket, []}, _env), do: {:ok, {:list, :any}, {:list, [], {:list, :any}}}
  # Cons expression: [h | t] → creates a list by prepending h to t
  defp check_impl({:bracket, {:cons, head, tail}}, env) do
    with {:ok, head_type, typed_head} <- check(head, env),
         {:ok, tail_type, typed_tail} <- check(tail, env) do
      # The result is a list containing head_type elements
      elem_type = case tail_type do
        {:list, t} -> unify_two_simple(head_type, t)
        _ -> head_type  # If tail type unknown, use head type
      end
      {:ok, {:list, elem_type}, {:cons, typed_head, typed_tail, {:list, elem_type}}}
    end
  end
  # Non-empty bracket with cons pattern: [h | t] → this is a pattern, not an expression
  # (Should only appear in match clauses, not as standalone expression)
  defp check_impl({:bracket, elements}, env) when is_list(elements) do
    # Treat as list literal: [1 2 3] → (list 1 2 3)
    check({:list, elements}, env)
  end

  # Do block: (do expr1 expr2 ...) → type of last expression
  defp check_impl({:do, []}, _env), do: {:ok, :unit, {:do, [], :unit}}
  defp check_impl({:do, exprs}, env) do
    case check_exprs_sequence(exprs, env) do
      {:ok, typed_exprs, last_type} ->
        {:ok, last_type, {:do, typed_exprs, last_type}}
      error -> error
    end
  end

  # String and unit literals (tuple forms)
  defp check_impl({:string, s}, _env), do: {:ok, :string, {:lit, :string, s}}
  defp check_impl({:unit, _loc}, _env), do: {:ok, :unit, {:lit, :unit, nil}}
  defp check_impl({:unit}, _env), do: {:ok, :unit, {:lit, :unit, nil}}

  # List literal: (list 1 2 3) → homogeneous list
  defp check_impl({:list, []}, _env), do: {:ok, {:list, :any}, {:list, [], {:list, :any}}}
  defp check_impl({:list, elements}, env) do
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
            {:error, Errors.list_type_mismatch(elem_type, mismatched)}
        end
      error -> error
    end
  end



  # Atom literal from parser: {:atom, :foo} → :foo
  # This is always a literal, never a variable lookup
  defp check_impl({:atom, a}, _env) when is_atom(a) do
    {:ok, {:atom, a}, {:lit, :atom, a}}
  end

  # Variable lookup
  # Distinguishes local variables from module-level function references
  defp check_impl({:var, name}, env) do
    case Map.get(env, name) do
      nil ->
        {:error, Errors.undefined_variable(name)}

      {:fn, params, _ret} = type ->
        # Function type - check if it's local or module-level
        if is_local_var?(name, env) do
          {:ok, type, {:var, name, type}}
        else
          # Module-level function - emit fn_ref so emitter can use &name/arity
          {:ok, type, {:fn_ref, name, length(params), type}}
        end

      type ->
        {:ok, type, {:var, name, type}}
    end
  end

  # Field access with row polymorphism: (. record :field) → field_type
  # Works with records, row types, and creates row constraints
  #
  # Row polymorphism allows functions to accept any record with at least
  # certain fields. For example:
  #   (defn get-name [r] (. r :name))
  # accepts any record with a :name field.
  #
  # When the record type is unknown (type variable or open row), we create
  # fresh type variables for the field type and row constraint. These are
  # deterministic based on the input IDs to ensure consistent types across
  # multiple accesses to the same field.
  defp check_impl({:field_access, record_expr, field}, env) when is_atom(field) do
    with {:ok, record_type, typed_record} <- check(record_expr, env) do
      case record_type do
        # Concrete record - look up field directly
        {:record, _name, fields} ->
          case List.keyfind(fields, field, 0) do
            {^field, field_type} ->
              {:ok, field_type, {:field_access, typed_record, field, field_type}}
            nil ->
              {:error, Errors.type_mismatch(
                {:row, [{field, :any}], {:rvar, 0}},
                record_type,
                note: "record does not have field `#{field}`"
              )}
          end

        # Row type - look up in known fields or constrain the tail
        {:row, fields, tail} ->
          case List.keyfind(fields, field, 0) do
            {^field, field_type} ->
              {:ok, field_type, {:field_access, typed_record, field, field_type}}
            nil ->
              # Field not in known fields - constrain the tail
              case tail do
                :closed ->
                  {:error, Errors.type_mismatch(
                    {:row, [{field, :any}], {:rvar, 0}},
                    record_type,
                    note: "closed row does not have field `#{field}`"
                  )}
                {:rvar, row_id} ->
                  # Create fresh type variable for the field type
                  # Use deterministic ID based on row_id and field name for consistency
                  field_tvar_id = fresh_field_tvar_id(row_id, field)
                  field_type = {:tvar, field_tvar_id}

                  # The row constraint: this row must have field => field_type
                  # plus whatever else the tail requires
                  row_constraint = {:row, [{field, field_type} | fields], {:rvar, row_id + 1}}

                  # Return the field type variable with row constraint in AST
                  {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}}
              end
          end

        # Type variable - convert to row type constraint
        {:tvar, tvar_id} ->
          # The record has an unknown type - constrain it to be a row with this field
          # Create fresh type variables for field type and row tail
          field_tvar_id = fresh_field_tvar_id(tvar_id, field)
          field_type = {:tvar, field_tvar_id}

          # Row constraint: tvar must be a row with at least this field
          row_constraint = {:row, [{field, field_type}], {:rvar, tvar_id}}

          {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}}

        :any ->
          # Treat :any as an open row type - enables row polymorphism for untyped params
          # Use a large base ID (2_000_000) to avoid collisions with regular tvars
          field_tvar_id = fresh_field_tvar_id(2_000_000, field)
          field_type = {:tvar, field_tvar_id}

          # Row constraint: any must be a row with at least this field
          row_constraint = {:row, [{field, field_type}], {:rvar, 2_000_000}}

          {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}}

        other ->
          {:error, Errors.type_mismatch(
            {:row, [{field, :any}], {:rvar, 0}},
            other,
            note: "field access requires a record or row type"
          )}
      end
    end
  end

  # Special form: spawn - returns a typed PID
  # (spawn process_name initial_state) → Pid<ProcessName>
  defp check_impl({:call, :spawn, [process_name, init_state]}, env) do
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
  defp check_impl({:call, :head, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, elem_type} ->
          {:ok, elem_type, {:call, :head, [typed_list], elem_type}}
        :any ->
          {:ok, :any, {:call, :head, [typed_list], :any}}
        other ->
          {:error, Errors.not_a_list(:head, other)}
      end
    end
  end

  # tail: (list a) → (list a)
  defp check_impl({:call, :tail, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _elem_type} = t ->
          {:ok, t, {:call, :tail, [typed_list], t}}
        :any ->
          {:ok, {:list, :any}, {:call, :tail, [typed_list], {:list, :any}}}
        other ->
          {:error, Errors.not_a_list(:tail, other)}
      end
    end
  end

  # cons: a → (list a) → (list a)
  defp check_impl({:call, :cons, [elem_expr, list_expr]}, env) do
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
            {:error, Errors.cons_type_mismatch(elem_type, list_type)}
          end
        :any ->
          result_type = {:list, elem_type}
          {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}}
        other ->
          {:error, Errors.not_a_list(:cons, other)}
      end
    end
  end

  # empty?: (list a) → bool
  defp check_impl({:call, :empty?, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _} ->
          {:ok, :bool, {:call, :empty?, [typed_list], :bool}}
        :any ->
          {:ok, :bool, {:call, :empty?, [typed_list], :bool}}
        other ->
          {:error, Errors.not_a_list(:empty?, other)}
      end
    end
  end

  # length: (list a) → int
  defp check_impl({:call, :length, [list_expr]}, env) do
    with {:ok, list_type, typed_list} <- check(list_expr, env) do
      case list_type do
        {:list, _} ->
          {:ok, :int, {:call, :length, [typed_list], :int}}
        :any ->
          {:ok, :int, {:call, :length, [typed_list], :int}}
        other ->
          {:error, Errors.not_a_list(:length, other)}
      end
    end
  end

  # str: variadic string concatenation/conversion
  # (str arg1 arg2 ...) → string
  # Converts each argument to string and concatenates them
  defp check_impl({:call, :str, args}, env) when is_list(args) and length(args) > 0 do
    # Type check all arguments - str accepts any type
    results = Enum.map(args, &check(&1, env))

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        typed_args = Enum.map(results, fn {:ok, _, ast} -> ast end)
        {:ok, :string, {:call, :str, typed_args, :string}}
    end
  end

  # --- Higher-order list functions ---

  # map: (a → b) → (list a) → (list b)
  # (map func list) - applies func to each element
  # Named function version
  # Skip built-in handling if user defined their own map function
  defp check_impl({:call, :map, [func_name, list_expr]}, env) when is_atom(func_name) do
    # If map is user-defined in env (not the built-in), use generic call handling
    case Map.get(env, :map) do
      nil -> check_builtin_map(func_name, list_expr, env)
      _user_defined -> check_generic_call(:map, [func_name, list_expr], env)
    end
  end



  # map with anonymous function (either 3-tuple or 4-tuple with location)
  defp check_impl({:call, :map, [{:fn, _, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :map) do
      nil -> check({:call, :map, [strip_fn_loc(fn_expr), list_expr]}, env)
      _user_defined -> check_generic_call(:map, [fn_expr, list_expr], env)
    end
  end
  defp check_impl({:call, :map, [{:fn, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :map) do
      nil -> check_builtin_map_anon(fn_expr, list_expr, env)
      _user_defined -> check_generic_call(:map, [fn_expr, list_expr], env)
    end
  end



  # filter: (a → bool) → (list a) → (list a)
  # (filter predicate list) - keeps elements where predicate returns true
  # Named function version - skip built-in if user defined filter
  defp check_impl({:call, :filter, [func_name, list_expr]}, env) when is_atom(func_name) do
    case Map.get(env, :filter) do
      nil -> check_builtin_filter(func_name, list_expr, env)
      _user_defined -> check_generic_call(:filter, [func_name, list_expr], env)
    end
  end



  # filter with anonymous function (either 3-tuple or 4-tuple with location)
  defp check_impl({:call, :filter, [{:fn, _, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :filter) do
      nil -> check({:call, :filter, [strip_fn_loc(fn_expr), list_expr]}, env)
      _user_defined -> check_generic_call(:filter, [fn_expr, list_expr], env)
    end
  end
  defp check_impl({:call, :filter, [{:fn, _, _} = fn_expr, list_expr]}, env) do
    case Map.get(env, :filter) do
      nil -> check_builtin_filter_anon(fn_expr, list_expr, env)
      _user_defined -> check_generic_call(:filter, [fn_expr, list_expr], env)
    end
  end



  # fold: (b → a → b) → b → (list a) → b
  # (fold func init list) - left fold
  # Named function version - skip built-in if user defined fold
  defp check_impl({:call, :fold, [func_name, init_expr, list_expr]}, env) when is_atom(func_name) do
    case Map.get(env, :fold) do
      nil -> check_builtin_fold(func_name, init_expr, list_expr, env)
      _user_defined -> check_generic_call(:fold, [func_name, init_expr, list_expr], env)
    end
  end



  # fold with anonymous function (either 3-tuple or 4-tuple with location)
  defp check_impl({:call, :fold, [{:fn, _, _, _} = fn_expr, init_expr, list_expr]}, env) do
    case Map.get(env, :fold) do
      nil -> check({:call, :fold, [strip_fn_loc(fn_expr), init_expr, list_expr]}, env)
      _user_defined -> check_generic_call(:fold, [fn_expr, init_expr, list_expr], env)
    end
  end
  defp check_impl({:call, :fold, [{:fn, _, _} = fn_expr, init_expr, list_expr]}, env) do
    case Map.get(env, :fold) do
      nil -> check_builtin_fold_anon(fn_expr, init_expr, list_expr, env)
      _user_defined -> check_generic_call(:fold, [fn_expr, init_expr, list_expr], env)
    end
  end



  # Special form: send (!) - validates message against typed PID
  # (! pid message) → :ok, but only if message is valid for that PID
  defp check_impl({:call, :"!", [pid_expr, msg_expr]}, env) do
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
            {:error, Errors.invalid_message(process_name, msg_atom, accepted_msgs)}
          end

        :pid ->
          # Untyped PID - allow any message (backward compat)
          {:ok, :ok, {:call, :"!", [typed_pid, typed_msg], :ok}}

        other ->
          {:error, Errors.send_to_non_pid(other)}
      end
    end
  end

  # Special form: unsafe send (!!) - sends to any PID without message validation
  # (!! pid message) → :ok, requires PID but skips message type checking
  # Use this for remote PIDs or when the PID's message type is unknown
  defp check_impl({:call, :"!!", [pid_expr, msg_expr]}, env) do
    with {:ok, pid_type, typed_pid} <- check(pid_expr, env),
         {:ok, _msg_type, typed_msg} <- check(msg_expr, env) do
      case pid_type do
        # Case 1: Typed PID - we know the process type, but we deliberately
        # skip message validation. The underscore _ ignores the process name
        # and accepted messages — we don't care what they are.
        {:pid, _process_name, _accepted_msgs} ->
          {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}}

        # Case 2: Untyped PID - a raw PID without type information.
        # This happens when PIDs come from external sources or parameters.
        :pid ->
          {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}}

        # Case 3: :any type - this is the "truly unsafe" case.
        # The parameter has no type annotation, so we allow it.
        # This is the whole point of !! — trusting the developer.
        :any ->
          {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}}

        # Case 4: Not a PID at all - this is still an error!
        # We catch obvious mistakes like (!! 42 :msg) or (!! "string" :msg).
        # The "unsafe" part is about messages, not about accepting garbage.
        other ->
          {:error, Errors.send_to_non_pid(other)}
      end
    end
  end

  # If expression: (if cond then else)
  defp check_impl({:if, condition, then_branch, else_branch}, env) do
    with {:ok, cond_type, typed_cond} <- check(condition, env),
         :ok <- expect_bool(cond_type),
         {:ok, then_type, typed_then} <- check(then_branch, env),
         {:ok, else_type, typed_else} <- check(else_branch, env),
         :ok <- expect_same_type(then_type, else_type) do
      {:ok, then_type, {:if, typed_cond, typed_then, typed_else, then_type}}
    end
  end

  # Match expression: (match expr [pattern body] ...)
  defp check_impl({:match, expr, clauses}, env) do
    with {:ok, expr_type, typed_expr} <- check(expr, env),
         {:ok, result_type, typed_clauses} <- check_match_clauses(clauses, expr_type, env) do
      {:ok, result_type, {:match, typed_expr, typed_clauses, result_type}}
    end
  end

  # Receive expression: (receive [pattern body] ...)
  # Blocks until a message matching one of the patterns arrives
  # Patterns are typed as :any (full typed PIDs would constrain this)
  defp check_impl({:receive, clauses}, env) do
    with {:ok, result_type, typed_clauses} <- check_receive_clauses(clauses, env) do
      {:ok, result_type, {:receive, typed_clauses, result_type}}
    end
  end

  # Let binding: (let [x 1 y 2] body)
  # Each binding extends the env for subsequent bindings and body
  defp check_impl({:let, bindings, body}, env) do
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

  # Qualified call: (erlang:hd xs) or (Vaisto.TypeChecker.Core/empty-subst)
  # Must come before generic function call to match first
  defp check_impl({:call, {:qualified, mod, func}, args}, env) do
    # Look up the extern in environment using "mod:func" key
    extern_name = :"#{mod}:#{func}"
    case Map.get(env, extern_name) do
      nil ->
        # Not registered - allow with :any return type (dynamic typing for interop)
        # This enables cross-module calls without explicit externs
        with {:ok, _arg_types, typed_args} <- check_args(args, env) do
          {:ok, :any, {:call, {:qualified, mod, func}, typed_args, :any}}
        end
      {:fn, _param_types, ret_type} ->
        with {:ok, _arg_types, typed_args} <- check_args(args, env) do
          # Note: we're not enforcing arg type checking for externs yet
          {:ok, ret_type, {:call, {:qualified, mod, func}, typed_args, ret_type}}
        end
      other ->
        {:error, Errors.extern_not_a_function(mod, func, other)}
    end
  end

  # Helper for generic function call handling (used when user overrides built-ins)

  # Arithmetic operators with numeric type widening
  # Handles: +, -, *, / with int/float/num type hierarchy
  defp check_impl({:call, op, [left, right]}, env) when op in [:+, :-, :*] do
    with {:ok, left_type, typed_left} <- check(left, env),
         {:ok, right_type, typed_right} <- check(right, env),
         {:ok, result_type} <- check_numeric_op(op, left_type, right_type) do
      {:ok, result_type, {:call, op, [typed_left, typed_right], result_type}}
    end
  end

  # Division always returns float
  defp check_impl({:call, :/, [left, right]}, env) do
    with {:ok, left_type, typed_left} <- check(left, env),
         {:ok, right_type, typed_right} <- check(right, env),
         :ok <- expect_numeric(left_type, "division"),
         :ok <- expect_numeric(right_type, "division") do
      {:ok, :float, {:call, :/, [typed_left, typed_right], :float}}
    end
  end

  # Comparison operators with numeric type support
  defp check_impl({:call, op, [left, right]}, env) when op in [:<, :>, :<=, :>=] do
    with {:ok, left_type, typed_left} <- check(left, env),
         {:ok, right_type, typed_right} <- check(right, env),
         :ok <- expect_numeric(left_type, "comparison"),
         :ok <- expect_numeric(right_type, "comparison") do
      {:ok, :bool, {:call, op, [typed_left, typed_right], :bool}}
    end
  end

  # Function call (general case)
  # If func is a local variable holding a function, emit {:apply, var, args, type}
  # If func is a module-level function, emit {:call, name, args, type}
  defp check_impl({:call, func, args}, env) when is_atom(func) do
    with {:ok, func_type} <- lookup_function(func, env),
         {:ok, arg_types, typed_args} <- check_args(args, env),
         {:ok, ret_type} <- unify_call(func_type, arg_types, args) do
      # Check if this is a local variable (function parameter or let-binding)
      # vs a module-level defn. Local vars have {:fn, ...} type directly in env.
      # We check if it's a "local" by seeing if it came from :__local_vars__
      if is_local_var?(func, env) do
        # Local function variable - needs f.(args) syntax in Elixir
        {:ok, ret_type, {:apply, {:var, func, func_type}, typed_args, ret_type}}
      else
        # Module-level function - uses func(args) syntax
        {:ok, ret_type, {:call, func, typed_args, ret_type}}
      end
    end
  end

  defp check_impl({:call, func, args}, env) do
    with {:ok, func_type} <- lookup_function(func, env),
         {:ok, arg_types, typed_args} <- check_args(args, env),
         {:ok, ret_type} <- unify_call(func_type, arg_types, args) do
      {:ok, ret_type, {:call, func, typed_args, ret_type}}
    end
  end

  # Process definition
  defp check_impl({:process, name, initial_state, handlers}, env) do
    with {:ok, state_type, _} <- check(initial_state, env),
         {:ok, typed_handlers} <- check_handlers(handlers, state_type, env) do
      process_type = {:process, state_type, handler_types(handlers)}
      {:ok, process_type, {:process, name, initial_state, typed_handlers, process_type}}
    end
  end

  # Supervision tree
  defp check_impl({:supervise, strategy, children}, env) do
    with :ok <- validate_strategy(strategy),
         {:ok, typed_children} <- check_children(children, env) do
      {:ok, :supervisor, {:supervise, strategy, typed_children, :supervisor}}
    end
  end

  # Product type (record): (deftype Point [x :int y :int])
  # Registers a constructor function and a record type
  defp check_impl({:deftype, name, {:product, fields}}, _env) do
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
  #
  # Type parameters are collected and made into type variables.
  # For example: (deftype Option (Some a) (None))
  # - Collects type param 'a' -> {:tvar, 0}
  # - Some constructor: ({:tvar, 0}) -> Option
  # - None constructor: () -> Option
  #
  # This enables type inference to propagate concrete types through constructors.
  defp check_impl({:deftype, name, {:sum, variants}}, _env) do
    # Collect all type parameters used across variants
    all_params = variants
    |> Enum.flat_map(fn {_ctor, params} -> params end)
    |> Enum.uniq()

    # Create type variable mapping: param_name -> {:tvar, index}
    param_map = all_params
    |> Enum.with_index()
    |> Map.new(fn {param, idx} -> {param, {:tvar, idx}} end)

    # Normalize variants with type variables instead of :any
    normalized_variants = Enum.map(variants, fn {ctor_name, type_params} ->
      field_types = Enum.map(type_params, fn param ->
        Map.get(param_map, param, :any)
      end)
      {ctor_name, field_types}
    end)

    sum_type = {:sum, name, normalized_variants}
    {:ok, sum_type, {:deftype, name, {:sum, normalized_variants}, sum_type}}
  end

  # Legacy support: old-style deftype without wrapper
  defp check_impl({:deftype, name, fields}, env) when is_list(fields) do
    check({:deftype, name, {:product, fields}}, env)
  end

  # Function definition: (defn add [x :int y :int] (+ x y))
  # Params are now [{:x, :int}, {:y, :int}] tuples with types
  defp check_impl({:defn, name, params, body, declared_ret_type}, env) do
    # Extract param names and types
    param_names = Enum.map(params, fn {n, _t} -> n end)
    param_types = Enum.map(params, fn {_n, t} -> t end)
    param_env = Map.new(params)

    # Add the function itself to env for recursion
    self_type = {:fn, param_types, declared_ret_type}
    # Mark parameters as local vars so they use f.(x) syntax if called as functions
    extended_env = env
      |> Map.merge(param_env)
      |> Map.put(name, self_type)
      |> then(fn e -> Enum.reduce(param_names, e, &add_local_var(&2, &1)) end)

    case check(body, extended_env) do
      {:ok, inferred_ret_type, typed_body} ->
        # If return type was declared, verify it matches
        if declared_ret_type != :any and not types_match?(declared_ret_type, inferred_ret_type) do
          {:error, Errors.return_type_mismatch(declared_ret_type, inferred_ret_type)}
        else
          final_ret_type = if declared_ret_type != :any, do: declared_ret_type, else: inferred_ret_type
          func_type = {:fn, param_types, final_ret_type}
          {:ok, func_type, {:defn, name, param_names, typed_body, func_type}}
        end
      error -> error
    end
  end

  # Legacy form without return type annotation
  defp check_impl({:defn, name, params, body}, env) do
    check({:defn, name, params, body, :any}, env)
  end

  # Value binding: (def name value)
  # Creates a module-level constant
  defp check_impl({:defval, name, value}, env) do
    case check(value, env) do
      {:ok, val_type, typed_value} ->
        {:ok, val_type, {:defval, name, typed_value, val_type}}
      error -> error
    end
  end

  # Anonymous function: (fn [x] (* x 2))
  # Uses Hindley-Milner inference to determine parameter types
  defp check_impl({:fn, params, body}, env) do
    # Try inference first for better type information
    case Vaisto.TypeSystem.Infer.infer({:fn, params, body}, env) do
      {:ok, func_type, typed_ast} ->
        {:ok, func_type, typed_ast}

      {:error, _} ->
        # Fall back to :any for params if inference fails
        # Handle pattern parameters by extracting their bindings
        {typed_params, param_bindings} = Enum.map_reduce(params, [], fn
          {:tuple_pattern, _elements} = pattern, acc ->
            # Extract variable bindings from the tuple pattern
            bindings = extract_pattern_bindings(pattern, :any, env)
            typed_pattern = type_pattern(pattern, :any, env)
            {typed_pattern, bindings ++ acc}
          var, acc when is_atom(var) and var not in [:_, true, false] ->
            {{:var, var, :any}, [{var, :any} | acc]}
          other, acc ->
            {other, acc}
        end)

        param_types = Enum.map(params, fn _ -> :any end)
        param_env = Map.new(param_bindings)
        extended_env = Map.merge(env, param_env)

        case check(body, extended_env) do
          {:ok, ret_type, typed_body} ->
            func_type = {:fn, param_types, ret_type}
            {:ok, func_type, {:fn, typed_params, typed_body, func_type}}
          error -> error
        end
    end
  end

  # Multi-clause function definition
  # (defn len [[] 0] [[h | t] (+ 1 (len t))])
  defp check_impl({:defn_multi, name, clauses}, env) do
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

    # Infer parameter types from patterns
    param_types = infer_multi_clause_param_types(clauses, arity)

    # Add function to env for recursion (with inferred param types)
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
        # Unify return types from all clauses
        ret_types = Enum.map(typed_clauses, fn {_, _, ret_type} -> ret_type end)
        unified_ret_type = unify_types_simple(ret_types)
        func_type = {:fn, param_types, unified_ret_type}
        {:ok, func_type, {:defn_multi, name, arity, Enum.reverse(typed_clauses), func_type}}
      error -> error
    end
  end

  # Record construction: (point 1 2) when point is a record type
  # Handled in function call - lookup_function returns the constructor type

  # Extern declaration: (extern erlang:hd [:any] :any)
  # Registers the function signature in the environment
  defp check_impl({:extern, mod, func, arg_types, ret_type}, _env) do
    # Parse type expressions (e.g., {:call, :List, [:any]} → {:list, :any})
    parsed_arg_types = Enum.map(arg_types, &parse_type_expr/1)
    parsed_ret_type = parse_type_expr(ret_type)
    func_type = {:fn, parsed_arg_types, parsed_ret_type}
    {:ok, :extern, {:extern, mod, func, func_type}}
  end

  # Module declaration: (ns MyModule)
  # Compile-time only - sets the module name for the current file
  defp check_impl({:ns, name}, _env) do
    {:ok, :ns, {:ns, name}}
  end

  # Import declaration: (import Std.List) or (import Std.List :as L)
  # Compile-time only - brings another module's exports into scope
  # The actual loading of the interface is done by the build system
  defp check_impl({:import, module, alias_name}, _env) do
    {:ok, :import, {:import, module, alias_name}}
  end

  # Handle parse errors (propagate them as type errors with location info)
  # Parse error - location is stripped by generic handler and added via with_loc
  defp check_impl({:error, msg}, _env) do
    {:error, Errors.parse_error(msg)}
  end

  # Parse type expressions from extern declarations and type annotations
  # Atom-wrapped type from parser: {:atom, :int} → :int
  defp parse_type_expr({:atom, t}) when is_atom(t), do: t
  # Simple types: :int, :any, :string
  defp parse_type_expr(t) when is_atom(t), do: t
  # List type: {:call, :List, [elem_type]} → {:list, elem_type}
  defp parse_type_expr({:call, :List, [elem_type]}), do: {:list, parse_type_expr(elem_type)}
  # List type with location: {:call, :List, [elem_type], loc} → {:list, elem_type}
  defp parse_type_expr({:call, :List, [elem_type], _loc}), do: {:list, parse_type_expr(elem_type)}
  # Fallback
  defp parse_type_expr(other), do: other

  # Generate a deterministic type variable ID for a field access
  # This ensures that accessing the same field on the same row variable
  # produces the same type variable, enabling proper constraint solving
  defp fresh_field_tvar_id(base_id, field) do
    # Use a large offset plus hash to avoid collisions with user-defined tvars
    # The hash ensures different fields get different IDs
    field_hash = :erlang.phash2(field, 1000)
    1_000_000 + base_id * 1000 + field_hash
  end

  # Strip location from fn AST nodes
  defp strip_fn_loc({:fn, params, body, %Vaisto.Parser.Loc{}}), do: {:fn, params, body}
  defp strip_fn_loc(other), do: other

  # Add location to error messages
  # Pass through success results, enhance errors with line/column
  defp with_loc({:ok, _, _} = result, _loc), do: result

  # Structured error - add location info if not already present
  defp with_loc({:error, %Error{primary_span: nil} = error}, %Vaisto.Parser.Loc{} = loc) do
    span = Error.span_from_loc(loc)
    {:error, %{error | file: loc.file, primary_span: span}}
  end
  defp with_loc({:error, %Error{} = error}, %Vaisto.Parser.Loc{} = loc) do
    # Error already has span, just add file if missing
    {:error, %{error | file: error.file || loc.file}}
  end

  # Legacy string error - convert to structured Error with location
  defp with_loc({:error, msg}, %Vaisto.Parser.Loc{} = loc) when is_binary(msg) do
    # Check if error already has location (format: "line:col:" or "file:line:col:")
    if String.match?(msg, ~r/^\d+:\d+:/) or String.match?(msg, ~r/^[^:]+:\d+:\d+:/) do
      # Error already has location, convert to Error struct as-is
      {:error, Error.from_string(msg)}
    else
      # Add location to the error
      span = Error.span_from_loc(loc)
      {:error, %Error{message: msg, file: loc.file, primary_span: span}}
    end
  end

  # Helper functions

  # Map literal: #{ :key val :key2 val2 }
  # Creates a row type with known fields
  defp check_map_literal(pairs, env) do
    case check_map_pairs(pairs, env, [], []) do
      {:ok, field_types, typed_pairs} ->
        # Row type with closed (no tail variable)
        map_type = {:row, field_types, :closed}
        {:ok, map_type, {:map, typed_pairs, map_type}}
      error -> error
    end
  end

  defp check_map_pairs([], _env, field_types, typed_pairs) do
    {:ok, Enum.reverse(field_types), Enum.reverse(typed_pairs)}
  end
  defp check_map_pairs([{key_expr, val_expr} | rest], env, field_types, typed_pairs) do
    with {:ok, _key_type, typed_key} <- check(key_expr, env),
         {:ok, val_type, typed_val} <- check(val_expr, env) do
      # Extract atom key for row type field
      field_name = case typed_key do
        {:lit, :atom, a} -> a
        _ -> :dynamic  # Non-atom keys get dynamic field name
      end
      check_map_pairs(rest, env, [{field_name, val_type} | field_types], [{typed_key, typed_val} | typed_pairs])
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
          {:error, Errors.mapper_arity(:map, 1, length(args))}
        {_, {:list, _}} ->
          {:error, Errors.not_a_function(:map, func_type)}
        {_, :any} ->
          {:error, Errors.not_a_function(:map, func_type)}
        {_, other} ->
          {:error, Errors.not_a_list(:map, other)}
      end
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
          {:error, Errors.mapper_arity(:map, 1, length(args))}
        {_, other} ->
          {:error, Errors.not_a_list(:map, other)}
      end
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
          {:error, Errors.predicate_not_bool(ret_type)}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, Errors.mapper_arity(:filter, 1, length(args))}
        {_, {:list, _}} ->
          {:error, Errors.not_a_function(:filter, func_type)}
        {_, :any} ->
          {:error, Errors.not_a_function(:filter, func_type)}
        {_, other} ->
          {:error, Errors.not_a_list(:filter, other)}
      end
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
          {:error, Errors.predicate_not_bool(ret_type)}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, Errors.mapper_arity(:filter, 1, length(args))}
        {_, other} ->
          {:error, Errors.not_a_list(:filter, other)}
      end
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
          {:error, Errors.mapper_arity(:fold, 2, length(args))}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}}
        {_, other} ->
          {:error, Errors.not_a_list(:fold, other)}
      end
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
          {:error, Errors.mapper_arity(:fold, 2, length(args))}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}}
        {_, other} ->
          {:error, Errors.not_a_list(:fold, other)}
      end
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

  defp lookup_function(name, env) do
    case Map.get(env, name) do
      nil -> {:error, Errors.unknown_function(name)}
      type -> {:ok, type}
    end
  end

  defp lookup_process(name, env) when is_atom(name) do
    case Map.get(env, name) do
      {:process, _, _} = process_type -> {:ok, process_type}
      nil -> {:error, Errors.unknown_process(name)}
      other -> {:error, Errors.type_mismatch(:process, other,
                  note: "`#{name}` is not a process")}
    end
  end

  # Check if a name is a local variable (parameter or let-bound) vs module-level
  defp is_local_var?(name, env) do
    local_vars = Map.get(env, :__local_vars__, MapSet.new())
    MapSet.member?(local_vars, name)
  end

  # Add a name to the local vars set
  defp add_local_var(env, name) do
    local_vars = Map.get(env, :__local_vars__, MapSet.new())
    Map.put(env, :__local_vars__, MapSet.put(local_vars, name))
  end

  defp check_bindings([], env, acc) do
    {:ok, env, Enum.reverse(acc)}
  end

  # Simple variable binding: (let [x expr] ...)
  defp check_bindings([{name, expr} | rest], env, acc) when is_atom(name) do
    case check(expr, env) do
      {:ok, type, typed_expr} ->
        extended_env = env
          |> Map.put(name, type)
          |> add_local_var(name)
        check_bindings(rest, extended_env, [{name, typed_expr, type} | acc])
      error -> error
    end
  end

  # Tuple pattern destructuring: (let [{x y} expr] ...)
  defp check_bindings([{{:tuple_pattern, elements}, expr} | rest], env, acc) do
    case check(expr, env) do
      {:ok, _type, typed_expr} ->
        # Extract variable bindings from the tuple pattern
        bindings = extract_pattern_bindings({:tuple_pattern, elements}, :any, env)
        extended_env = Enum.reduce(bindings, env, fn {var, var_type}, e ->
          Map.put(e, var, var_type)
        end)
        # Type the pattern
        typed_pattern = type_pattern({:tuple_pattern, elements}, :any, env)
        check_bindings(rest, extended_env, [{typed_pattern, typed_expr, :any} | acc])
      error -> error
    end
  end

  # Cons pattern destructuring in let: (let [[head | tail] expr] ...)
  # Represented as {:bracket, {:cons, head, tail}}
  defp check_bindings([{{:bracket, {:cons, head, tail}}, expr} | rest], env, acc) do
    case check(expr, env) do
      {:ok, {:list, elem_type}, typed_expr} ->
        # head gets elem_type, tail gets list type
        extended_env = env
          |> Map.put(head, elem_type)
          |> add_local_var(head)
          |> Map.put(tail, {:list, elem_type})
          |> add_local_var(tail)
        typed_pattern = {:cons_pattern, head, tail, {:list, elem_type}}
        check_bindings(rest, extended_env, [{typed_pattern, typed_expr, {:list, elem_type}} | acc])
      {:ok, type, typed_expr} ->
        # Expression type not a list - use :any for elements
        extended_env = env
          |> Map.put(head, :any)
          |> add_local_var(head)
          |> Map.put(tail, {:list, :any})
          |> add_local_var(tail)
        typed_pattern = {:cons_pattern, head, tail, type}
        check_bindings(rest, extended_env, [{typed_pattern, typed_expr, type} | acc])
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
        {:error, Errors.non_exhaustive_sum(type_name, MapSet.to_list(missing))}
      end
    end
  end

  # For :any type with tuple patterns, check for non-exhaustive result-like matches
  # Only error on clearly incomplete patterns like matching {:ok v} without {:error e}
  # Other single-pattern tuple matches (like {:point x y}) are allowed
  defp check_exhaustiveness(clauses, :any) do
    has_catch_all = Enum.any?(clauses, fn {pattern, _body} ->
      is_catch_all_pattern?(pattern)
    end)

    if has_catch_all do
      :ok
    else
      # Check for result-like patterns: {:ok ...} without {:error ...} or vice versa
      tags = clauses
        |> Enum.map(fn {pattern, _body} -> extract_tuple_tag(pattern) end)
        |> Enum.filter(&(&1 != nil))
        |> MapSet.new()

      result_tags = MapSet.new([:ok, :error])
      has_result_tags = not MapSet.disjoint?(tags, result_tags)
      missing_result_tags = MapSet.difference(result_tags, tags)

      # Only error if using result-like tags without covering both
      if has_result_tags and MapSet.size(missing_result_tags) > 0 do
        {:error, Errors.non_exhaustive_result(MapSet.to_list(missing_result_tags))}
      else
        :ok
      end
    end
  end

  # Non-sum types don't need exhaustiveness checking (for now)
  defp check_exhaustiveness(_clauses, _expr_type), do: :ok
  # Extract variant constructor name from pattern
  defp extract_variant_name({:call, name, _, _}), do: name
  defp extract_variant_name({:pattern, name, _, _}), do: name
  defp extract_variant_name(_), do: nil

  # Extract tag (first element) from tuple pattern
  # Parser produces: {:atom, :ok} for atom literals
  # TypeChecker produces: {:lit, :atom, :ok} for typed literals
  defp extract_tuple_tag({:tuple, [{:lit, :atom, tag} | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple, [{:atom, tag} | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple, [tag | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [{:lit, :atom, tag} | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [{:atom, tag} | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [tag | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag(_), do: nil

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

  # Patterns against type variables or row types - delegate to :any path (env constructor lookup)
  defp extract_pattern_bindings({:call, _, _} = pattern, {:tvar, _}, env) do
    extract_pattern_bindings(pattern, :any, env)
  end

  defp extract_pattern_bindings({:call, _, _} = pattern, {:row, _, _}, env) do
    extract_pattern_bindings(pattern, :any, env)
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

  # Tuple pattern: {:tuple_pattern, elements} - extract bindings from all elements
  defp extract_pattern_bindings({:tuple_pattern, elements}, _type, env) do
    Enum.flat_map(elements, fn el -> extract_pattern_bindings(el, :any, env) end)
  end

  # Tuple from parser with location info: {:tuple, elements, loc}
  defp extract_pattern_bindings({:tuple, elements, %Vaisto.Parser.Loc{}}, _type, env) do
    Enum.flat_map(elements, fn el -> extract_pattern_bindings(el, :any, env) end)
  end

  # Cons pattern: {:cons, head, tail} - extract bindings from head and tail
  defp extract_pattern_bindings({:cons, head, tail}, _type, env) do
    extract_pattern_bindings(head, :any, env) ++ extract_pattern_bindings(tail, :any, env)
  end

  # Bracket pattern (list inside pattern): {:bracket, elements}
  # elements can be a list [a, b, c] or a cons pattern {:cons, h, t}
  defp extract_pattern_bindings({:bracket, {:cons, _, _} = cons}, type, env) do
    extract_pattern_bindings(cons, type, env)
  end
  defp extract_pattern_bindings({:bracket, elements}, _type, env) when is_list(elements) do
    Enum.flat_map(elements, fn el -> extract_pattern_bindings(el, :any, env) end)
  end

  # Empty list pattern
  defp extract_pattern_bindings([], _type, _env), do: []

  # Atom literals in patterns don't bind
  defp extract_pattern_bindings({:atom, _}, _type, _env), do: []

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

  # Patterns against type variables or row types - delegate to :any path
  defp type_pattern({:call, _, _} = pattern, {:tvar, _}, env) do
    type_pattern(pattern, :any, env)
  end

  defp type_pattern({:call, _, _} = pattern, {:row, _, _}, env) do
    type_pattern(pattern, :any, env)
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

  # Empty list pattern: [] → {:list_pattern, [], :any}
  defp type_pattern([], _type, _env) do
    {:list_pattern, [], :any}
  end

  # Tuple pattern: {:tuple_pattern, elements} → {:tuple_pattern, typed_elements, :any}
  defp type_pattern({:tuple_pattern, elements}, _type, env) do
    typed_elements = Enum.map(elements, fn el -> type_pattern(el, :any, env) end)
    {:tuple_pattern, typed_elements, :any}
  end

  # Tuple from parser with location info: {:tuple, elements, loc}
  defp type_pattern({:tuple, elements, %Vaisto.Parser.Loc{}}, _type, env) do
    typed_elements = Enum.map(elements, fn el -> type_pattern(el, :any, env) end)
    {:tuple_pattern, typed_elements, :any}
  end

  # Cons pattern: {:cons, head, tail} → {:cons_pattern, typed_head, typed_tail, :any}
  defp type_pattern({:cons, head, tail}, _type, env) do
    typed_head = type_pattern(head, :any, env)
    typed_tail = type_pattern(tail, :any, env)
    {:cons_pattern, typed_head, typed_tail, :any}
  end

  # Bracket pattern (list inside pattern): {:bracket, elements} → list pattern
  # elements can be a list [a, b, c] or a cons pattern {:cons, h, t}
  defp type_pattern({:bracket, {:cons, _, _} = cons}, type, env) do
    type_pattern(cons, type, env)
  end
  defp type_pattern({:bracket, elements}, _type, env) when is_list(elements) do
    typed_elements = Enum.map(elements, fn el -> type_pattern(el, :any, env) end)
    {:list_pattern, typed_elements, :any}
  end

  defp type_pattern(var, type, _env) when is_atom(var) and var not in [:_, true, false] do
    {:var, var, type}
  end

  defp type_pattern(lit, _type, _env) when is_integer(lit) or is_float(lit) or is_atom(lit) do
    lit
  end

  # String literal patterns
  defp type_pattern({:string, s}, _type, _env) when is_binary(s) do
    {:string, s, :string}
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
  # Bracket with elements: {:bracket, elements}
  defp extract_multi_pattern_bindings({:bracket, []}) do
    []
  end
  defp extract_multi_pattern_bindings({:bracket, elements}) when is_list(elements) do
    Enum.flat_map(elements, &extract_multi_pattern_bindings/1)
  end
  # Bracket with cons: {:bracket, {:cons, h, t}}
  defp extract_multi_pattern_bindings({:bracket, {:cons, _, _} = cons}) do
    extract_multi_pattern_bindings(cons)
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
  # Bracket empty list: {:bracket, []} → {:list, [], {:list, :any}}
  defp type_multi_pattern({:bracket, []}, _env) do
    {:list, [], {:list, :any}}
  end
  # Bracket with elements: {:bracket, elements} → list pattern
  defp type_multi_pattern({:bracket, elements}, env) when is_list(elements) do
    typed_elements = Enum.map(elements, &type_multi_pattern(&1, env))
    {:list, typed_elements, {:list, :any}}
  end
  # Bracket with cons: {:bracket, {:cons, h, t}} → cons pattern
  defp type_multi_pattern({:bracket, {:cons, _, _} = cons}, env) do
    type_multi_pattern(cons, env)
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

  # Check a sequence of expressions, return typed list and type of last expression
  defp check_exprs_sequence([], _env), do: {:ok, [], :unit}
  defp check_exprs_sequence(exprs, env) do
    results = Enum.map(exprs, &check(&1, env))

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        typed = Enum.map(results, fn {:ok, _, ast} -> ast end)
        {_, last_type, _} = List.last(results)
        {:ok, typed, last_type}
    end
  end

  # Header clause to define default for original_args
  defp unify_call(func_type, actual_args, original_args \\ [])

  defp unify_call({:fn, expected_args, ret_type}, actual_args, original_args) do
    if length(expected_args) != length(actual_args) do
      {:error, Errors.arity_mismatch(:function, length(expected_args), length(actual_args))}
    else
      mismatches =
        Enum.zip([expected_args, actual_args, original_args ++ List.duplicate(nil, length(actual_args))])
        |> Enum.with_index()
        |> Enum.filter(fn {{exp, act, _}, _} -> not types_match?(exp, act) end)

      case mismatches do
        [] -> {:ok, ret_type}
        [{{expected, actual, orig_arg}, idx} | _] ->
          # Extract location from original argument if available
          span_opts = case extract_loc(orig_arg) do
            nil -> []
            loc -> [span: Error.span_from_loc(loc)]
          end
          {:error, Errors.type_mismatch(expected, actual,
            Keyword.merge(span_opts, [note: "at argument #{idx + 1}"]))}
      end
    end
  end

  # When the function type is :any (untyped higher-order function parameter)
  # allow any arguments and return :any
  defp unify_call(:any, _actual_args, _original_args) do
    {:ok, :any}
  end

  # Extract location from AST node (location is always the last element of tuple AST nodes)
  defp extract_loc(nil), do: nil
  defp extract_loc(tuple) when is_tuple(tuple) do
    last = elem(tuple, tuple_size(tuple) - 1)
    case last do
      %Vaisto.Parser.Loc{} = loc -> loc
      _ -> nil
    end
  end
  defp extract_loc(_), do: nil

  defp types_match?(:any, _), do: true
  defp types_match?(_, :any), do: true
  # Type variables match anything (for parametric polymorphism)
  defp types_match?({:tvar, _}, _), do: true
  defp types_match?(_, {:tvar, _}), do: true
  defp types_match?(t, t), do: true
  # Singleton atoms match each other (they unify to :atom)
  defp types_match?({:atom, _}, {:atom, _}), do: true
  defp types_match?({:atom, _}, :atom), do: true
  defp types_match?(:atom, {:atom, _}), do: true
  # Numeric subtyping (directional):
  # - :int and :float are subtypes of :num (can pass int/float where num expected)
  # - :int is NOT a subtype of :float (can't pass int where float expected - debatable)
  # - :float is NOT a subtype of :int (can't pass float where int expected)
  # expected = :num accepts actual = :int or :float
  defp types_match?(:num, :int), do: true
  defp types_match?(:num, :float), do: true
  # But NOT: types_match?(:int, :float) or types_match?(:float, :int)
  # List covariance: {:list, :int} matches {:list, :any}
  defp types_match?({:list, elem1}, {:list, elem2}), do: types_match?(elem1, elem2)
  # Function types: contravariant in args, covariant in return
  defp types_match?({:fn, args1, ret1}, {:fn, args2, ret2}) when length(args1) == length(args2) do
    args_match = Enum.zip(args1, args2) |> Enum.all?(fn {a1, a2} -> types_match?(a2, a1) end)
    args_match and types_match?(ret1, ret2)
  end
  # Record types with same name match if fields match
  defp types_match?({:record, name, fields1}, {:record, name, fields2}) do
    Enum.all?(fields1, fn {field_name, type1} ->
      case List.keyfind(fields2, field_name, 0) do
        {^field_name, type2} -> types_match?(type1, type2)
        nil -> false
      end
    end)
  end
  # Sum types with same name always match
  defp types_match?({:sum, name, _}, {:sum, name, _}), do: true
  defp types_match?(_, _), do: false

  defp expect_bool(:bool), do: :ok
  defp expect_bool(:any), do: :ok
  defp expect_bool(other), do: {:error, Errors.type_mismatch(:bool, other, hint: "conditions must be boolean")}

  # Check that a type is numeric (int, float, or num)
  defp expect_numeric(:int, _op), do: :ok
  defp expect_numeric(:float, _op), do: :ok
  defp expect_numeric(:num, _op), do: :ok
  defp expect_numeric(:any, _op), do: :ok
  defp expect_numeric({:tvar, _}, _op), do: :ok  # Type variables are assumed numeric in polymorphic contexts
  defp expect_numeric(other, op), do: {:error, Errors.type_mismatch(:num, other, hint: "#{op} requires numeric operands")}

  # Determine result type for arithmetic operations
  # int op int → int
  # float op float → float
  # int op float or float op int → float
  # any involving :num → :num
  defp check_numeric_op(_op, :int, :int), do: {:ok, :int}
  defp check_numeric_op(_op, :float, :float), do: {:ok, :float}
  defp check_numeric_op(_op, :int, :float), do: {:ok, :float}
  defp check_numeric_op(_op, :float, :int), do: {:ok, :float}
  defp check_numeric_op(_op, :num, :num), do: {:ok, :num}
  defp check_numeric_op(_op, :num, :int), do: {:ok, :num}
  defp check_numeric_op(_op, :int, :num), do: {:ok, :num}
  defp check_numeric_op(_op, :num, :float), do: {:ok, :num}
  defp check_numeric_op(_op, :float, :num), do: {:ok, :num}
  defp check_numeric_op(_op, :any, t) when t in [:int, :float, :num, :any], do: {:ok, :num}
  defp check_numeric_op(_op, t, :any) when t in [:int, :float, :num, :any], do: {:ok, :num}
  # Type variables in polymorphic contexts - return :num as the result
  defp check_numeric_op(_op, {:tvar, _}, _), do: {:ok, :num}
  defp check_numeric_op(_op, _, {:tvar, _}), do: {:ok, :num}
  defp check_numeric_op(op, t1, t2) do
    {:error, Errors.type_mismatch(:num, t1, hint: "#{op} requires numeric operands, got #{inspect(t1)} and #{inspect(t2)}")}
  end

  # Unify two types for branch expressions - returns the unified type or error
  # This is used when branches need to return the same type (if, match, cond)
  defp unify_types(t, t), do: {:ok, t}
  defp unify_types(:any, t), do: {:ok, t}
  defp unify_types(t, :any), do: {:ok, t}
  # Singleton atoms unify to universal :atom
  defp unify_types({:atom, _}, {:atom, _}), do: {:ok, :atom}
  defp unify_types({:atom, _}, :atom), do: {:ok, :atom}
  defp unify_types(:atom, {:atom, _}), do: {:ok, :atom}
  # Numeric type widening: int + float → float
  defp unify_types(:int, :float), do: {:ok, :float}
  defp unify_types(:float, :int), do: {:ok, :float}
  defp unify_types(:int, :num), do: {:ok, :num}
  defp unify_types(:num, :int), do: {:ok, :num}
  defp unify_types(:float, :num), do: {:ok, :num}
  defp unify_types(:num, :float), do: {:ok, :num}
  # List types unify if element types unify
  defp unify_types({:list, t1}, {:list, t2}) do
    case unify_types(t1, t2) do
      {:ok, unified} -> {:ok, {:list, unified}}
      error -> error
    end
  end
  # Type variables unify with anything
  defp unify_types({:tvar, _} = t, _), do: {:ok, t}
  defp unify_types(_, {:tvar, _} = t), do: {:ok, t}
  # Same sum types unify
  defp unify_types({:sum, name, _} = t, {:sum, name, _}), do: {:ok, t}
  # Otherwise, error
  defp unify_types(t1, t2), do: {:error, Errors.branch_type_mismatch(t1, t2)}

  defp expect_same_type(t1, t2) do
    case unify_types(t1, t2) do
      {:ok, _unified} -> :ok
      error -> error
    end
  end

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
  defp validate_strategy(strategy), do: {:error, Errors.unknown_supervision_strategy(strategy)}

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
    Logger.debug("typecheck: checking module with #{length(forms)} forms")
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

        # Value binding: infer type from the value expression
        # Note: This doesn't support recursive values (use defn for those)
        {:defval, name, value, %Vaisto.Parser.Loc{}} ->
          collect_defval_signature(name, value, acc_env)

        {:defval, name, value} ->
          collect_defval_signature(name, value, acc_env)

        _ ->
          acc_env
      end
    end)

    # Second pass: type-check each form with full environment
    check_module_forms(forms, env_with_signatures, [], [])
  end

  # Signature collection helpers
  defp collect_defn_signature(name, params, ret_type, env) do
    # Params are now [{:x, :int}, {:y, :int}] tuples
    param_types = Enum.map(params, fn {_name, type} -> type end)
    func_type = {:fn, param_types, ret_type}
    Map.put(env, name, func_type)
  end

  defp collect_defval_signature(name, value, env) do
    # Type-check the value expression to determine its type
    # Note: This happens during signature collection, so the value
    # expression must not reference other defvals (no circular deps)
    case check(value, env) do
      {:ok, val_type, _typed_value} ->
        Map.put(env, name, val_type)
      {:error, _} ->
        # If type checking fails, use :any to allow forward progress
        # The error will be caught again during the full check pass
        Map.put(env, name, :any)
    end
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
    # Collect all type parameters used across variants
    all_params = variants
    |> Enum.flat_map(fn {_ctor, params} -> params end)
    |> Enum.uniq()

    # Create type variable mapping: param_name -> {:tvar, index}
    param_map = all_params
    |> Enum.with_index()
    |> Map.new(fn {param, idx} -> {param, {:tvar, idx}} end)

    # Normalize variants with type variables
    normalized_variants = Enum.map(variants, fn {ctor_name, type_params} ->
      field_types = Enum.map(type_params, fn param ->
        Map.get(param_map, param, :any)
      end)
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

  defp check_module_forms([], _env, acc, errors) do
    case errors do
      [] -> {:ok, :module, {:module, Enum.reverse(acc)}}
      _ -> {:error, Enum.reverse(errors)}
    end
  end

  defp check_module_forms([form | rest], env, acc, errors) do
    case check(form, env) do
      {:ok, _type, typed_form} ->
        # Update env with more precise types after checking
        new_env = update_env_from_typed_form(typed_form, env)
        check_module_forms(rest, new_env, [typed_form | acc], errors)

      {:error, err} ->
        # Continue checking remaining forms, accumulate error
        check_module_forms(rest, env, acc, [err | errors])
    end
  end

  # Extract env updates from a typed form
  defp update_env_from_typed_form(typed_form, env) do
    case typed_form do
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
  end

  # --- Multi-clause parameter type inference ---

  # Infer parameter types from multi-clause patterns
  # For list functions, we can tell the param is a list type
  # For variant matches, we can determine the sum type
  defp infer_multi_clause_param_types(clauses, arity) do
    # Look at all clause patterns to infer types
    pattern_types = Enum.map(clauses, fn {pattern, _body} ->
      infer_pattern_type(pattern)
    end)

    # Unify all pattern types to get the most specific type
    unified = unify_types_simple(pattern_types)
    List.duplicate(unified, arity)
  end

  # Infer type from a pattern
  defp infer_pattern_type([]), do: {:list, :any}
  defp infer_pattern_type({:cons, _, _}), do: {:list, :any}
  defp infer_pattern_type({:list, _}), do: {:list, :any}
  defp infer_pattern_type(elems) when is_list(elems), do: {:list, :any}
  defp infer_pattern_type(n) when is_integer(n), do: :int
  defp infer_pattern_type({:atom, _}), do: :atom
  defp infer_pattern_type({:call, _, _}), do: :any  # Record/variant pattern
  defp infer_pattern_type(_), do: :any

  # Simple type unification for multi-clause functions
  # Returns the most specific type that's compatible with all inputs
  defp unify_types_simple([]), do: :any
  defp unify_types_simple([t]), do: t
  defp unify_types_simple([t1 | rest]) do
    Enum.reduce(rest, t1, &unify_two_simple/2)
  end

  # Unify two types, returning the most specific compatible type
  defp unify_two_simple(:any, t), do: t
  defp unify_two_simple(t, :any), do: t
  defp unify_two_simple(t, t), do: t
  defp unify_two_simple({:list, e1}, {:list, e2}), do: {:list, unify_two_simple(e1, e2)}
  defp unify_two_simple({:fn, a1, r1}, {:fn, a2, r2}) when length(a1) == length(a2) do
    unified_args = Enum.zip(a1, a2) |> Enum.map(fn {x, y} -> unify_two_simple(x, y) end)
    {:fn, unified_args, unify_two_simple(r1, r2)}
  end
  defp unify_two_simple({:record, n, f1}, {:record, n, f2}) do
    # Same record name, unify fields
    {:record, n, unify_fields_simple(f1, f2)}
  end
  defp unify_two_simple({:sum, n, v1}, {:sum, n, v2}) when v1 == v2 do
    {:sum, n, v1}
  end
  defp unify_two_simple(_, _), do: :any  # Incompatible types fall back to :any

  defp unify_fields_simple(f1, f2) do

    map2 = Map.new(f2)
    Enum.map(f1, fn {name, type1} ->
      type2 = Map.get(map2, name, :any)
      {name, unify_two_simple(type1, type2)}
    end)
  end
end
