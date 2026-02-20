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
  alias Vaisto.TypeChecker.TcCtx
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
  # Routes through check_s which threads TcCtx for substitution propagation
  def check(node, env) when is_tuple(node) do
    ctx = TcCtx.new(env)
    case check_s(node, ctx) do
      {:ok, type, ast, ctx} ->
        final_type = TcCtx.apply_subst(ctx, type)
        final_ast = apply_subst_to_ast(ctx.subst, ast)
        {:ok, final_type, final_ast}
      {:error, _} = err -> err
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
  # Internal dispatch chain threading TcCtx (substitution context)
  # check_s mirrors check/2 but threads ctx; check_impl_s mirrors check_impl/2
  # ============================================================================

  # Tuple nodes: strip loc, dispatch to check_impl_s
  defp check_s(node, ctx) when is_tuple(node) do
    case Vaisto.LocationStripper.strip_node(node) do
      {^node, nil} ->
        check_impl_s(node, ctx)
      {stripped, %Vaisto.Parser.Loc{} = loc} ->
        case check_s(stripped, ctx) do
          {:ok, type, ast, ctx} -> {:ok, type, with_loc_ast(ast, loc), ctx}
          {:error, _} = err -> with_loc(err, loc)
        end
    end
  end

  # Literals — same as check/2 but return ctx
  defp check_s(n, ctx) when is_integer(n), do: {:ok, :int, {:lit, :int, n}, ctx}
  defp check_s(f, ctx) when is_float(f), do: {:ok, :float, {:lit, :float, f}, ctx}
  defp check_s(true, ctx), do: {:ok, :bool, {:lit, :bool, true}, ctx}
  defp check_s(false, ctx), do: {:ok, :bool, {:lit, :bool, false}, ctx}

  # Atoms
  defp check_s(a, ctx) when is_atom(a) do
    case check(a, ctx.env) do
      {:ok, type, ast} -> {:ok, type, ast, ctx}
      {:error, _} = err -> err
    end
  end

  # Fallback for non-tuple/non-literal
  defp check_s(other, ctx) do
    case check(other, ctx.env) do
      {:ok, type, ast} -> {:ok, type, ast, ctx}
      {:error, _} = err -> err
    end
  end

  # Typed AST doesn't carry location — this is a no-op placeholder
  defp with_loc_ast(ast, _loc), do: ast

  # === Migrated check_impl_s clauses ===
  # Each clause threads TcCtx. Direct check() calls → check_s() for ctx propagation.
  # Helper calls that internally recurse use ctx.env (helpers migrated in later phases).

  # Tuple patterns and tuples — ctx-threaded
  defp check_impl_s({:tuple_pattern, elements}, ctx) do
    case check_args_s(elements, ctx) do
      {:ok, _types, typed_elements, ctx} -> {:ok, :any, {:tuple, typed_elements, :any}, ctx}
      error -> error
    end
  end
  defp check_impl_s({:map, pairs}, ctx) do
    check_map_literal_s(pairs, ctx)
  end
  defp check_impl_s({:tuple, elements}, ctx) do
    case check_args_s(elements, ctx) do
      {:ok, _types, typed_elements, ctx} -> {:ok, :any, {:tuple, typed_elements, :any}, ctx}
      error -> error
    end
  end

  # Bracket expressions
  defp check_impl_s({:bracket, []}, ctx), do: {:ok, {:list, :any}, {:list, [], {:list, :any}}, ctx}

  defp check_impl_s({:bracket, {:cons, head, tail}}, ctx) do
    with {:ok, head_type, typed_head, ctx} <- check_s(head, ctx),
         {:ok, tail_type, typed_tail, ctx} <- check_s(tail, ctx) do
      elem_type = case tail_type do
        {:list, t} -> join_types(head_type, t)
        _ -> head_type
      end
      {:ok, {:list, elem_type}, {:cons, typed_head, typed_tail, {:list, elem_type}}, ctx}
    end
  end

  defp check_impl_s({:bracket, elements}, ctx) when is_list(elements) do
    check_s({:list, elements}, ctx)
  end

  # Do block
  defp check_impl_s({:do, []}, ctx), do: {:ok, :unit, {:do, [], :unit}, ctx}

  defp check_impl_s({:do, exprs}, ctx) do
    case check_exprs_sequence_s(exprs, ctx) do
      {:ok, typed_exprs, last_type, ctx} ->
        {:ok, last_type, {:do, typed_exprs, last_type}, ctx}
      error -> error
    end
  end

  # String and unit literals
  defp check_impl_s({:string, s}, ctx), do: {:ok, :string, {:lit, :string, s}, ctx}
  defp check_impl_s({:unit, _loc}, ctx), do: {:ok, :unit, {:lit, :unit, nil}, ctx}
  defp check_impl_s({:unit}, ctx), do: {:ok, :unit, {:lit, :unit, nil}, ctx}

  # List literal
  defp check_impl_s({:list, []}, ctx), do: {:ok, {:list, :any}, {:list, [], {:list, :any}}, ctx}

  defp check_impl_s({:list, elements}, ctx) do
    case check_args_s(elements, ctx) do
      {:ok, types, typed_elements, ctx} ->
        elem_type = Enum.reduce(types, :any, &join_types/2)
        {:ok, {:list, elem_type}, {:list, typed_elements, {:list, elem_type}}, ctx}
      error -> error
    end
  end

  # Atom literal
  defp check_impl_s({:atom, a}, ctx) when is_atom(a) do
    {:ok, {:atom, a}, {:lit, :atom, a}, ctx}
  end

  # Variable lookup
  defp check_impl_s({:var, name}, ctx) do
    case Map.get(ctx.env, name) do
      nil ->
        {:error, Errors.undefined_variable(name)}
      {:fn, params, _ret} = type ->
        if is_local_var?(name, ctx.env) do
          {:ok, type, {:var, name, type}, ctx}
        else
          {:ok, type, {:fn_ref, name, length(params), type}, ctx}
        end
      type ->
        {:ok, type, {:var, name, type}, ctx}
    end
  end

  # Field access with row polymorphism
  defp check_impl_s({:field_access, record_expr, field}, ctx) when is_atom(field) do
    with {:ok, record_type, typed_record, ctx} <- check_s(record_expr, ctx) do
      case record_type do
        {:record, _name, fields} ->
          case List.keyfind(fields, field, 0) do
            {^field, field_type} ->
              {:ok, field_type, {:field_access, typed_record, field, field_type}, ctx}
            nil ->
              {:error, Errors.type_mismatch(
                {:row, [{field, :any}], {:rvar, 0}},
                record_type,
                note: "record does not have field `#{field}`"
              )}
          end

        {:row, fields, tail} ->
          case List.keyfind(fields, field, 0) do
            {^field, field_type} ->
              {:ok, field_type, {:field_access, typed_record, field, field_type}, ctx}
            nil ->
              case tail do
                :closed ->
                  {:error, Errors.type_mismatch(
                    {:row, [{field, :any}], {:rvar, 0}},
                    record_type,
                    note: "closed row does not have field `#{field}`"
                  )}
                {:rvar, row_id} ->
                  field_tvar_id = fresh_field_tvar_id(row_id, field)
                  field_type = {:tvar, field_tvar_id}
                  row_constraint = {:row, [{field, field_type} | fields], {:rvar, row_id + 1}}
                  {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}, ctx}
              end
          end

        {:tvar, tvar_id} ->
          field_tvar_id = fresh_field_tvar_id(tvar_id, field)
          field_type = {:tvar, field_tvar_id}
          row_constraint = {:row, [{field, field_type}], {:rvar, tvar_id}}
          {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}, ctx}

        :any ->
          any_row_base_id = 200
          field_tvar_id = fresh_field_tvar_id(any_row_base_id, field)
          field_type = {:tvar, field_tvar_id}
          row_constraint = {:row, [{field, field_type}], {:rvar, any_row_base_id}}
          {:ok, field_type, {:field_access, typed_record, field, field_type, row_constraint}, ctx}

        other ->
          {:error, Errors.type_mismatch(
            {:row, [{field, :any}], {:rvar, 0}},
            other,
            note: "field access requires a record or row type"
          )}
      end
    end
  end

  # spawn
  defp check_impl_s({:call, :spawn, [process_name, init_state]}, ctx) do
    with {:ok, process_type} <- lookup_process(process_name, ctx.env),
         {:ok, _init_type, typed_init, ctx} <- check_s(init_state, ctx) do
      {:process, _state_type, accepted_msgs} = process_type
      pid_type = {:pid, process_name, accepted_msgs}
      {:ok, pid_type, {:call, :spawn, [process_name, typed_init], pid_type}, ctx}
    end
  end

  # head
  defp check_impl_s({:call, :head, [list_expr]}, ctx) do
    with {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case list_type do
        {:list, elem_type} -> {:ok, elem_type, {:call, :head, [typed_list], elem_type}, ctx}
        :any -> {:ok, :any, {:call, :head, [typed_list], :any}, ctx}
        other -> {:error, Errors.not_a_list(:head, other)}
      end
    end
  end

  # tail
  defp check_impl_s({:call, :tail, [list_expr]}, ctx) do
    with {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case list_type do
        {:list, _elem_type} = t -> {:ok, t, {:call, :tail, [typed_list], t}, ctx}
        :any -> {:ok, {:list, :any}, {:call, :tail, [typed_list], {:list, :any}}, ctx}
        other -> {:error, Errors.not_a_list(:tail, other)}
      end
    end
  end

  # cons
  defp check_impl_s({:call, :cons, [elem_expr, list_expr]}, ctx) do
    with {:ok, elem_type, typed_elem, ctx} <- check_s(elem_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case list_type do
        {:list, :any} ->
          result_type = {:list, elem_type}
          {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}, ctx}
        {:list, list_elem_type} ->
          if types_unifiable?(elem_type, list_elem_type) do
            result_type = {:list, list_elem_type}
            {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}, ctx}
          else
            {:error, Errors.cons_type_mismatch(elem_type, list_type)}
          end
        :any ->
          result_type = {:list, elem_type}
          {:ok, result_type, {:call, :cons, [typed_elem, typed_list], result_type}, ctx}
        other ->
          {:error, Errors.not_a_list(:cons, other)}
      end
    end
  end

  # empty?
  defp check_impl_s({:call, :empty?, [list_expr]}, ctx) do
    with {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case list_type do
        {:list, _} -> {:ok, :bool, {:call, :empty?, [typed_list], :bool}, ctx}
        :any -> {:ok, :bool, {:call, :empty?, [typed_list], :bool}, ctx}
        other -> {:error, Errors.not_a_list(:empty?, other)}
      end
    end
  end

  # length
  defp check_impl_s({:call, :length, [list_expr]}, ctx) do
    with {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case list_type do
        {:list, _} -> {:ok, :int, {:call, :length, [typed_list], :int}, ctx}
        :any -> {:ok, :int, {:call, :length, [typed_list], :int}, ctx}
        other -> {:error, Errors.not_a_list(:length, other)}
      end
    end
  end

  # str — variadic
  defp check_impl_s({:call, :str, args}, ctx) when is_list(args) and length(args) > 0 do
    case check_args_s(args, ctx) do
      {:ok, _types, typed_args, ctx} ->
        {:ok, :string, {:call, :str, typed_args, :string}, ctx}
      {:error, _} = err -> err
    end
  end

  # map: (a → b) → (list a) → (list b)
  defp check_impl_s({:call, :map, [func_name, list_expr]}, ctx) when is_atom(func_name) do
    case Map.get(ctx.env, :map) do
      nil -> check_builtin_map_s(func_name, list_expr, ctx)
      _user_defined -> check_generic_call_s(:map, [func_name, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :map, [{:fn, _, _, _} = fn_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :map) do
      nil -> check_s({:call, :map, [strip_fn_loc(fn_expr), list_expr]}, ctx)
      _user_defined -> check_generic_call_s(:map, [fn_expr, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :map, [{:fn, _, _} = fn_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :map) do
      nil -> check_builtin_map_anon_s(fn_expr, list_expr, ctx)
      _user_defined -> check_generic_call_s(:map, [fn_expr, list_expr], ctx)
    end
  end

  # filter: (a → bool) → (list a) → (list a)
  defp check_impl_s({:call, :filter, [func_name, list_expr]}, ctx) when is_atom(func_name) do
    case Map.get(ctx.env, :filter) do
      nil -> check_builtin_filter_s(func_name, list_expr, ctx)
      _user_defined -> check_generic_call_s(:filter, [func_name, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :filter, [{:fn, _, _, _} = fn_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :filter) do
      nil -> check_s({:call, :filter, [strip_fn_loc(fn_expr), list_expr]}, ctx)
      _user_defined -> check_generic_call_s(:filter, [fn_expr, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :filter, [{:fn, _, _} = fn_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :filter) do
      nil -> check_builtin_filter_anon_s(fn_expr, list_expr, ctx)
      _user_defined -> check_generic_call_s(:filter, [fn_expr, list_expr], ctx)
    end
  end

  # fold: (b → a → b) → b → (list a) → b
  defp check_impl_s({:call, :fold, [func_name, init_expr, list_expr]}, ctx) when is_atom(func_name) do
    case Map.get(ctx.env, :fold) do
      nil -> check_builtin_fold_s(func_name, init_expr, list_expr, ctx)
      _user_defined -> check_generic_call_s(:fold, [func_name, init_expr, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :fold, [{:fn, _, _, _} = fn_expr, init_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :fold) do
      nil -> check_s({:call, :fold, [strip_fn_loc(fn_expr), init_expr, list_expr]}, ctx)
      _user_defined -> check_generic_call_s(:fold, [fn_expr, init_expr, list_expr], ctx)
    end
  end
  defp check_impl_s({:call, :fold, [{:fn, _, _} = fn_expr, init_expr, list_expr]}, ctx) do
    case Map.get(ctx.env, :fold) do
      nil -> check_builtin_fold_anon_s(fn_expr, init_expr, list_expr, ctx)
      _user_defined -> check_generic_call_s(:fold, [fn_expr, init_expr, list_expr], ctx)
    end
  end

  # send (!)
  defp check_impl_s({:call, :"!", [pid_expr, msg_expr]}, ctx) do
    with {:ok, pid_type, typed_pid, ctx} <- check_s(pid_expr, ctx),
         {:ok, msg_type, typed_msg, ctx} <- check_s(msg_expr, ctx) do
      case pid_type do
        {:pid, process_name, accepted_msgs} ->
          msg_atom = case msg_type do
            {:atom, a} -> a
            _ -> nil
          end
          if msg_atom in accepted_msgs do
            {:ok, :ok, {:call, :"!", [typed_pid, typed_msg], :ok}, ctx}
          else
            {:error, Errors.invalid_message(process_name, msg_atom, accepted_msgs)}
          end
        :pid ->
          {:ok, :ok, {:call, :"!", [typed_pid, typed_msg], :ok}, ctx}
        other ->
          {:error, Errors.send_to_non_pid(other)}
      end
    end
  end

  # unsafe send (!!)
  defp check_impl_s({:call, :"!!", [pid_expr, msg_expr]}, ctx) do
    with {:ok, pid_type, typed_pid, ctx} <- check_s(pid_expr, ctx),
         {:ok, _msg_type, typed_msg, ctx} <- check_s(msg_expr, ctx) do
      case pid_type do
        {:pid, _, _} -> {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}, ctx}
        :pid -> {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}, ctx}
        :any -> {:ok, :ok, {:call, :"!!", [typed_pid, typed_msg], :ok}, ctx}
        other -> {:error, Errors.send_to_non_pid(other)}
      end
    end
  end

  # if expression
  defp check_impl_s({:if, condition, then_branch, else_branch}, ctx) do
    with {:ok, cond_type, typed_cond, ctx} <- check_s(condition, ctx),
         :ok <- expect_bool(cond_type),
         {:ok, then_type, typed_then, ctx} <- check_s(then_branch, ctx),
         {:ok, else_type, typed_else, ctx} <- check_s(else_branch, ctx),
         {:ok, ctx} <- expect_same_type_s(then_type, else_type, ctx) do
      {:ok, then_type, {:if, typed_cond, typed_then, typed_else, then_type}, ctx}
    end
  end

  # match expression
  defp check_impl_s({:match, expr, clauses}, ctx) do
    with {:ok, expr_type, typed_expr, ctx} <- check_s(expr, ctx),
         {:ok, result_type, typed_clauses, ctx} <- check_match_clauses_s(clauses, expr_type, ctx) do
      {:ok, result_type, {:match, typed_expr, typed_clauses, result_type}, ctx}
    end
  end

  # receive expression
  defp check_impl_s({:receive, clauses}, ctx) do
    with {:ok, result_type, typed_clauses, ctx} <- check_receive_clauses_s(clauses, ctx) do
      {:ok, result_type, {:receive, typed_clauses, result_type}, ctx}
    end
  end

  # let binding
  defp check_impl_s({:let, bindings, body}, ctx) do
    case check_bindings_s(bindings, ctx, []) do
      {:ok, ctx, typed_bindings} ->
        case check_s(body, ctx) do
          {:ok, body_type, typed_body, ctx} ->
            {:ok, body_type, {:let, typed_bindings, typed_body, body_type}, ctx}
          error -> error
        end
      error -> error
    end
  end

  # Qualified call
  defp check_impl_s({:call, {:qualified, mod, func}, args}, ctx) do
    extern_name = :"#{mod}:#{func}"
    case Map.get(ctx.env, extern_name) do
      nil ->
        with {:ok, _arg_types, typed_args, ctx} <- check_args_s(args, ctx) do
          {:ok, :any, {:call, {:qualified, mod, func}, typed_args, :any}, ctx}
        end
      {:fn, _param_types, ret_type} ->
        with {:ok, _arg_types, typed_args, ctx} <- check_args_s(args, ctx) do
          {:ok, ret_type, {:call, {:qualified, mod, func}, typed_args, ret_type}, ctx}
        end
      other ->
        {:error, Errors.extern_not_a_function(mod, func, other)}
    end
  end

  # Arithmetic operators
  defp check_impl_s({:call, op, [left, right]}, ctx) when op in [:+, :-, :*] do
    with {:ok, left_type, typed_left, ctx} <- check_s(left, ctx),
         {:ok, right_type, typed_right, ctx} <- check_s(right, ctx),
         {:ok, result_type} <- check_numeric_op(op, left_type, right_type) do
      {:ok, result_type, {:call, op, [typed_left, typed_right], result_type}, ctx}
    end
  end

  # Division
  defp check_impl_s({:call, :/, [left, right]}, ctx) do
    with {:ok, left_type, typed_left, ctx} <- check_s(left, ctx),
         {:ok, right_type, typed_right, ctx} <- check_s(right, ctx),
         :ok <- expect_numeric(left_type, "division"),
         :ok <- expect_numeric(right_type, "division") do
      {:ok, :float, {:call, :/, [typed_left, typed_right], :float}, ctx}
    end
  end

  # Comparison operators
  defp check_impl_s({:call, op, [left, right]}, ctx) when op in [:<, :>, :<=, :>=] do
    with {:ok, left_type, typed_left, ctx} <- check_s(left, ctx),
         {:ok, right_type, typed_right, ctx} <- check_s(right, ctx),
         :ok <- expect_numeric(left_type, "comparison"),
         :ok <- expect_numeric(right_type, "comparison") do
      {:ok, :bool, {:call, op, [typed_left, typed_right], :bool}, ctx}
    end
  end

  # Function call (atom) — full ctx threading with unify_call_s
  defp check_impl_s({:call, func, args}, ctx) when is_atom(func) do
    with {:ok, func_type} <- lookup_function(func, ctx.env) do
      case func_type do
        {:constrained_method, vars, constraints, fn_type, method_name} ->
          check_class_method_call_s(method_name, vars, constraints, fn_type, args, ctx)

        {:forall, vars, fn_type} ->
          base_id = :erlang.unique_integer([:positive, :monotonic]) * 1000
          fresh_subst = vars
            |> Enum.with_index()
            |> Map.new(fn {var, idx} -> {var, {:tvar, base_id + idx}} end)
          inst_fn_type = Vaisto.TypeSystem.Core.apply_subst(fresh_subst, fn_type)

          with {:ok, arg_types, typed_args, ctx} <- check_args_s(args, ctx),
               {:ok, ret_type, ctx} <- unify_call_s(inst_fn_type, arg_types, ctx, args) do
            {:ok, ret_type, {:call, func, typed_args, ret_type}, ctx}
          end

        _ ->
          with {:ok, arg_types, typed_args, ctx} <- check_args_s(args, ctx),
               {:ok, ret_type, ctx} <- unify_call_s(func_type, arg_types, ctx, args) do
            if is_local_var?(func, ctx.env) do
              {:ok, ret_type, {:apply, {:var, func, func_type}, typed_args, ret_type}, ctx}
            else
              {:ok, ret_type, {:call, func, typed_args, ret_type}, ctx}
            end
          end
      end
    end
  end

  # Function call (expression) — full ctx threading
  defp check_impl_s({:call, func, args}, ctx) do
    with {:ok, func_type} <- lookup_function(func, ctx.env),
         {:ok, arg_types, typed_args, ctx} <- check_args_s(args, ctx),
         {:ok, ret_type, ctx} <- unify_call_s(func_type, arg_types, ctx, args) do
      {:ok, ret_type, {:call, func, typed_args, ret_type}, ctx}
    end
  end

  # Process definition
  defp check_impl_s({:process, name, initial_state, handlers}, ctx) do
    with {:ok, state_type, _, ctx} <- check_s(initial_state, ctx),
         {:ok, typed_handlers, ctx} <- check_handlers_s(handlers, state_type, ctx) do
      process_type = {:process, state_type, handler_types(handlers)}
      {:ok, process_type, {:process, name, initial_state, typed_handlers, process_type}, ctx}
    end
  end

  # Supervision tree
  defp check_impl_s({:supervise, strategy, children}, ctx) do
    with :ok <- validate_strategy(strategy),
         {:ok, typed_children} <- check_children(children, ctx.env) do
      {:ok, :supervisor, {:supervise, strategy, typed_children, :supervisor}, ctx}
    end
  end

  # Product type (record)
  defp check_impl_s({:deftype, name, {:product, fields}}, ctx) do
    normalized_fields = Enum.map(fields, fn {field_name, type} ->
      {field_name, parse_type_expr(type)}
    end)
    record_type = {:record, name, normalized_fields}
    {:ok, record_type, {:deftype, name, {:product, normalized_fields}, record_type}, ctx}
  end

  # Sum type (ADT)
  defp check_impl_s({:deftype, name, {:sum, variants}}, ctx) do
    all_params = variants
      |> Enum.flat_map(fn {_ctor, params} -> params end)
      |> Enum.uniq()
    param_map = all_params
      |> Enum.with_index()
      |> Map.new(fn {param, idx} -> {param, {:tvar, idx}} end)
    normalized_variants = Enum.map(variants, fn {ctor_name, type_params} ->
      field_types = Enum.map(type_params, fn param ->
        Map.get(param_map, param, :any)
      end)
      {ctor_name, field_types}
    end)
    sum_type = {:sum, name, normalized_variants}
    {:ok, sum_type, {:deftype, name, {:sum, normalized_variants}, sum_type}, ctx}
  end

  # Legacy deftype
  defp check_impl_s({:deftype, name, fields}, ctx) when is_list(fields) do
    check_s({:deftype, name, {:product, fields}}, ctx)
  end

  # Function definition with return type
  defp check_impl_s({:defn, name, params, body, declared_ret_type}, ctx) do
    param_names = Enum.map(params, fn {n, _t} -> n end)
    param_types = Enum.map(params, fn {_n, t} -> t end)
    param_env = Map.new(params)

    self_type = {:fn, param_types, declared_ret_type}
    extended_env = ctx.env
      |> Map.merge(param_env)
      |> Map.put(name, self_type)
      |> then(fn e -> Enum.reduce(param_names, e, &add_local_var(&2, &1)) end)

    extended_ctx = %{ctx | env: extended_env}

    case check_s(body, extended_ctx) do
      {:ok, inferred_ret_type, typed_body, ctx} ->
        if declared_ret_type != :any and not types_unifiable?(declared_ret_type, inferred_ret_type) do
          {:error, Errors.return_type_mismatch(declared_ret_type, inferred_ret_type)}
        else
          final_ret_type = if declared_ret_type != :any, do: declared_ret_type, else: inferred_ret_type
          func_type = {:fn, param_types, final_ret_type}
          {:ok, func_type, {:defn, name, param_names, typed_body, func_type}, ctx}
        end
      error -> error
    end
  end

  # Legacy defn without return type
  defp check_impl_s({:defn, name, params, body}, ctx) do
    check_s({:defn, name, params, body, :any}, ctx)
  end

  # Value binding
  defp check_impl_s({:defval, name, value}, ctx) do
    case check_s(value, ctx) do
      {:ok, val_type, typed_value, ctx} ->
        {:ok, val_type, {:defval, name, typed_value, val_type}, ctx}
      error -> error
    end
  end

  # Anonymous function — uses Infer first, falls back to manual check
  defp check_impl_s({:fn, params, body}, ctx) do
    case Vaisto.TypeSystem.Infer.infer({:fn, params, body}, ctx.env) do
      {:ok, func_type, typed_ast} ->
        {:ok, func_type, typed_ast, ctx}

      {:error, _} ->
        {typed_params, param_bindings} = Enum.map_reduce(params, [], fn
          {:tuple_pattern, _elements} = pattern, acc ->
            bindings = extract_pattern_bindings(pattern, :any, ctx.env)
            typed_pattern = type_pattern(pattern, :any, ctx.env)
            {typed_pattern, bindings ++ acc}
          var, acc when is_atom(var) and var not in [:_, true, false] ->
            {{:var, var, :any}, [{var, :any} | acc]}
          other, acc ->
            {other, acc}
        end)

        param_types = Enum.map(params, fn _ -> :any end)
        extended_ctx = %{ctx | env: Map.merge(ctx.env, Map.new(param_bindings))}

        case check_s(body, extended_ctx) do
          {:ok, ret_type, typed_body, ctx} ->
            func_type = {:fn, param_types, ret_type}
            {:ok, func_type, {:fn, typed_params, typed_body, func_type}, ctx}
          error -> error
        end
    end
  end

  # Multi-clause function definition
  defp check_impl_s({:defn_multi, name, clauses}, ctx) do
    {first_pattern, _} = hd(clauses)
    arity = case first_pattern do
      [] -> 1
      {:cons, _, _} -> 1
      {:list, _} -> 1
      {:call, _, args} -> length(args)
      _ -> 1
    end

    param_types = infer_multi_clause_param_types(clauses, arity)
    self_type = {:fn, param_types, :any}
    extended_ctx = %{ctx | env: Map.put(ctx.env, name, self_type)}

    typed_clauses_result = Enum.reduce_while(clauses, {:ok, [], extended_ctx}, fn {pattern, body}, {:ok, acc, ctx} ->
      bindings = extract_multi_pattern_bindings(pattern)
      clause_ctx = %{ctx | env: Enum.reduce(bindings, ctx.env, fn {var, type}, e ->
        Map.put(e, var, type)
      end)}

      case check_s(body, clause_ctx) do
        {:ok, body_type, typed_body, ctx} ->
          typed_pattern = type_multi_pattern(pattern, ctx.env)
          {:cont, {:ok, [{typed_pattern, typed_body, body_type} | acc], ctx}}
        error ->
          {:halt, error}
      end
    end)

    case typed_clauses_result do
      {:ok, typed_clauses, ctx} ->
        ret_types = Enum.map(typed_clauses, fn {_, _, ret_type} -> ret_type end)
        unified_ret_type = join_types_list(ret_types)
        func_type = {:fn, param_types, unified_ret_type}
        {:ok, func_type, {:defn_multi, name, arity, Enum.reverse(typed_clauses), func_type}, ctx}
      error -> error
    end
  end

  # Extern declaration
  defp check_impl_s({:extern, mod, func, arg_types, ret_type}, ctx) do
    parsed_arg_types = Enum.map(arg_types, &parse_type_expr/1)
    parsed_ret_type = parse_type_expr(ret_type)
    func_type = {:fn, parsed_arg_types, parsed_ret_type}
    {:ok, :extern, {:extern, mod, func, func_type}, ctx}
  end

  # Module declaration
  defp check_impl_s({:ns, name}, ctx), do: {:ok, :ns, {:ns, name}, ctx}

  # Import declaration
  defp check_impl_s({:import, module, alias_name}, ctx), do: {:ok, :import, {:import, module, alias_name}, ctx}

  # Type class declaration
  defp check_impl_s({:defclass, class_name, type_params, methods}, ctx) do
    classes = Map.get(ctx.env, :__classes__, %{})
    case Map.get(classes, class_name) do
      {:class, _, _, _, _} ->
        {:ok, :defclass, {:defclass, class_name, type_params, methods, :defclass}, ctx}
      {:class, _, _, _} ->
        {:ok, :defclass, {:defclass, class_name, type_params, methods, :defclass}, ctx}
      nil ->
        {:error, Errors.unknown_expression({:defclass, class_name})}
    end
  end

  # Instance and constrained instance: delegate method body checking to old check_impl
  defp check_impl_s({:instance, class_name, for_type, methods}, ctx) do
    case check_impl({:instance, class_name, for_type, methods}, ctx.env) do
      {:ok, type, ast} -> {:ok, type, ast, ctx}
      {:error, _} = err -> err
    end
  end

  defp check_impl_s({:instance_constrained, class_name, type_name, type_params, constraints, methods}, ctx) do
    case check_impl({:instance_constrained, class_name, type_name, type_params, constraints, methods}, ctx.env) do
      {:ok, type, ast} -> {:ok, type, ast, ctx}
      {:error, _} = err -> err
    end
  end

  defp check_impl_s({:deftype_deriving, name, type_def, classes}, ctx) do
    case check_impl({:deftype_deriving, name, type_def, classes}, ctx.env) do
      {:ok, type, ast} -> {:ok, type, ast, ctx}
      {:error, _} = err -> err
    end
  end

  # Parse error propagation
  defp check_impl_s({:error, %Error{} = error}, _ctx), do: {:error, error}
  defp check_impl_s({:error, msg}, _ctx) when is_binary(msg), do: {:error, Errors.parse_error(msg)}

  # ============================================================================
  # Legacy check_impl clauses — retained for instance/deriving bodies only
  # These call check(body, local_env) which re-enters through check/2 → check_s
  # ============================================================================

  defp check_impl({:instance, class_name, for_type, methods}, env) do
    classes = Map.get(env, :__classes__, %{})
    case Map.get(classes, class_name) do
      class_def when is_tuple(class_def) and elem(class_def, 0) == :class ->
        {tvar_ids, method_sigs, defaults} = extract_class_parts(class_def)

        # Check for missing methods, accounting for defaults
        required_names = Enum.map(method_sigs, fn {name, _type} -> name end) |> MapSet.new()
        provided_names = Enum.map(methods, fn {name, _params, _body} -> name end) |> MapSet.new()
        missing = MapSet.difference(required_names, provided_names)

        # Split missing into those with defaults and truly missing
        {with_defaults, truly_missing} = Enum.split_with(missing, fn name ->
          Map.has_key?(defaults, name)
        end)

        if length(truly_missing) > 0 do
          {:error, Error.new("instance #{class_name} #{inspect(for_type)} is missing methods: #{Enum.join(truly_missing, ", ")}")}
        else
          # Inject default bodies for missing methods
          injected = Enum.map(with_defaults, fn name ->
            {:default, param_names, body} = Map.fetch!(defaults, name)
            {name, param_names, body}
          end)
          all_methods = methods ++ injected

          # Type-check each method body with the concrete type
          # Resolve ADT name atom to full type for substitution
          resolved_type = resolve_instance_type(for_type, env)
          subst = Map.new(tvar_ids, fn id -> {id, resolved_type} end)
          method_sig_map = Map.new(method_sigs)

          typed_methods = Enum.map(all_methods, fn {method_name, params, body} ->
            expected_type = Vaisto.TypeSystem.Core.apply_subst(subst, method_sig_map[method_name])
            {:fn, param_types, _ret_type} = expected_type

            # Build local env with params bound to their concrete types
            local_env = Enum.zip(params, param_types)
              |> Enum.reduce(env, fn {param, type}, acc ->
                acc |> Map.put(param, type) |> add_local_var(param)
              end)

            case check(body, local_env) do
              {:ok, _body_type, typed_body} ->
                {method_name, params, typed_body}
              {:error, _} = err ->
                throw(err)
            end
          end)

          # Sort typed_methods to match class definition order (for dictionary indexing)
          class_order = Enum.map(method_sigs, fn {name, _} -> name end)
          sorted_methods = Enum.sort_by(typed_methods, fn {name, _, _} ->
            Enum.find_index(class_order, &(&1 == name)) || 999
          end)

          {:ok, :instance, {:instance, class_name, for_type, sorted_methods, :instance}}
        end

      nil ->
        {:error, Error.new("unknown type class: #{class_name}")}
    end
  catch
    {:error, _} = err -> err
  end

  # Constrained type class instance: (instance Show (Maybe a) where [(Show a)] ...)
  defp check_impl({:instance_constrained, class_name, type_name, type_params, constraints, methods}, env) do
    classes = Map.get(env, :__classes__, %{})
    case Map.get(classes, class_name) do
      class_def when is_tuple(class_def) and elem(class_def, 0) == :class ->
        {tvar_ids, method_sigs, defaults} = extract_class_parts(class_def)

        # Check for missing methods, accounting for defaults
        required_names = Enum.map(method_sigs, fn {name, _type} -> name end) |> MapSet.new()
        provided_names = Enum.map(methods, fn {name, _params, _body} -> name end) |> MapSet.new()
        missing = MapSet.difference(required_names, provided_names)
        {with_defaults, truly_missing} = Enum.split_with(missing, fn name ->
          Map.has_key?(defaults, name)
        end)

        if length(truly_missing) > 0 do
          {:error, Error.new("instance #{class_name} #{type_name} is missing methods: #{Enum.join(truly_missing, ", ")}")}
        else
          injected = Enum.map(with_defaults, fn name ->
            {:default, param_names, body} = Map.fetch!(defaults, name)
            {name, param_names, body}
          end)
          all_methods = methods ++ injected

          # Build parameterized type: resolve Maybe → {:sum, :Maybe, [{:Just, [{:tvar, N}]}, ...]}
          # then map tvars → named type params (:a, :b, ...)
          resolved_type = resolve_instance_type(type_name, env)
          tvar_to_param = build_tvar_to_param_mapping(resolved_type, type_params)
          parameterized_type = apply_tvar_to_param(resolved_type, tvar_to_param)

          # Substitute class tvars with parameterized type
          subst = Map.new(tvar_ids, fn id -> {id, parameterized_type} end)
          method_sig_map = Map.new(method_sigs)

          # Register virtual instances for constraints
          env_with_virtuals = case register_virtual_instances(constraints, env) do
            {:error, _} = err -> throw(err)
            {:ok, env_v} -> env_v
          end

          typed_methods = Enum.map(all_methods, fn {method_name, params, body} ->
            expected_type = Vaisto.TypeSystem.Core.apply_subst(subst, method_sig_map[method_name])
            {:fn, param_types, _ret_type} = expected_type

            local_env = Enum.zip(params, param_types)
              |> Enum.reduce(env_with_virtuals, fn {param, type}, acc ->
                acc |> Map.put(param, type) |> add_local_var(param)
              end)

            case check(body, local_env) do
              {:ok, _body_type, typed_body} ->
                {method_name, params, typed_body}
              {:error, _} = err ->
                throw(err)
            end
          end)

          # Sort typed_methods to match class definition order
          class_order = Enum.map(method_sigs, fn {name, _} -> name end)
          sorted_methods = Enum.sort_by(typed_methods, fn {name, _, _} ->
            Enum.find_index(class_order, &(&1 == name)) || 999
          end)

          {:ok, :instance, {:instance_constrained, class_name, type_name, type_params, constraints, sorted_methods, :instance}}
        end

      nil ->
        {:error, Error.new("unknown type class: #{class_name}")}
    end
  catch
    {:error, _} = err -> err
  end

  # Standalone deftype_deriving (single expression, not in a module)
  defp check_impl({:deftype_deriving, name, type_def, classes}, env) do
    loc = %Vaisto.Parser.Loc{}
    case check({:deftype, name, type_def, loc}, env) do
      {:ok, _type, typed_deftype} ->
        new_env = update_env_from_typed_form(typed_deftype, env)
        results = Enum.map(classes, fn class ->
          synthesize_and_check_instance(class, name, type_def, loc, new_env)
        end)
        case Enum.find(results, &match?({:error, _}, &1)) do
          {:error, _} = err -> err
          nil ->
            typed_instances = Enum.map(results, fn {:ok, _t, typed} -> typed end)
            {:ok, :module, {:module, [typed_deftype | typed_instances]}}
        end
      {:error, _} = err -> err
    end
  end

  # Build mapping from tvar IDs in a resolved type to named type params
  defp build_tvar_to_param_mapping({:sum, _name, variants}, type_params) do
    tvar_ids = variants
      |> Enum.flat_map(fn {_, types} -> collect_tvar_ids(types) end)
      |> Enum.uniq()
    Enum.zip(tvar_ids, type_params) |> Map.new()
  end
  defp build_tvar_to_param_mapping(_other, _type_params), do: %{}

  # Replace tvar IDs with named atoms (type params) in a type
  defp apply_tvar_to_param({:tvar, id}, mapping) do
    Map.get(mapping, id, {:tvar, id})
  end
  defp apply_tvar_to_param({:sum, name, variants}, mapping) do
    {:sum, name, Enum.map(variants, fn {ctor, types} ->
      {ctor, Enum.map(types, &apply_tvar_to_param(&1, mapping))}
    end)}
  end
  defp apply_tvar_to_param({:fn, params, ret}, mapping) do
    {:fn, Enum.map(params, &apply_tvar_to_param(&1, mapping)), apply_tvar_to_param(ret, mapping)}
  end
  defp apply_tvar_to_param({:list, t}, mapping), do: {:list, apply_tvar_to_param(t, mapping)}
  defp apply_tvar_to_param(other, _mapping), do: other

  # Register virtual instances for constraints during constrained instance body checking
  defp register_virtual_instances(constraints, env) do
    Enum.with_index(constraints)
    |> Enum.reduce_while({:ok, env}, fn {{class_name, tvar}, idx}, {:ok, acc} ->
      classes = Map.get(acc, :__classes__, %{})
      class_def = Map.get(classes, class_name)
      case class_def do
        nil ->
          {:halt, {:error, Error.new("unknown type class `#{class_name}` in constraint")}}
        _ ->
          {class_tvar_ids, method_sigs, _} = extract_class_parts(class_def)
          # Build method types substituting class tvar → constraint tvar (the named param)
          subst = Map.new(class_tvar_ids, fn id -> {id, tvar} end)
          concrete = Map.new(method_sigs, fn {name, type} ->
            {name, Vaisto.TypeSystem.Core.apply_subst(subst, type)}
          end)
          virtual = {:virtual, idx, concrete}
          instances = Map.get(acc, :__instances__, %{})
          {:cont, {:ok, Map.put(acc, :__instances__, Map.put(instances, {class_name, tvar}, virtual))}}
      end
    end)
  end

  # Extract tvar_ids, method_sigs, defaults from class def (4-tuple or 5-tuple)
  defp extract_class_parts({:class, _name, tvar_ids, method_sigs, defaults}),
    do: {tvar_ids, method_sigs, defaults}
  defp extract_class_parts({:class, _name, tvar_ids, method_sigs}),
    do: {tvar_ids, method_sigs, %{}}

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
  # ID space partitioning:
  #   0          .. 9_999         : Parser-assigned tvars
  #   10_000     .. 99_999_999    : fresh_tvar_id (unique_integer + 10_000)
  #   100_000_000 .. +∞           : fresh_field_tvar_id (deterministic, hash-based)
  @field_tvar_base 100_000_000
  defp fresh_field_tvar_id(base_id, field) do
    field_hash = :erlang.phash2(field, 1_000_000)
    @field_tvar_base + base_id * 1_000_000 + field_hash
  end

  # Strip location from fn AST nodes
  defp strip_fn_loc({:fn, params, body, %Vaisto.Parser.Loc{}}), do: {:fn, params, body}
  defp strip_fn_loc(other), do: other

  # Add location to error messages — enhance errors with line/column

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


  defp resolve_class_constraints(constraints, method_name, ret_type, typed_args, arg_types, env) do
    instances = Map.get(env, :__instances__, %{})

    # For each constraint, try to resolve the concrete type, accumulating results
    Enum.reduce_while(constraints, {:ok, []}, fn {class_name, constraint_type}, {:ok, acc} ->
      # The constraint type should be concrete after unification with args
      concrete_type = resolve_constraint_type(constraint_type, arg_types)
      instance_key = normalize_instance_type(concrete_type)

      cond do
        # Still a type variable — can't resolve yet, emit as regular call
        match?({:tvar, _}, concrete_type) ->
          {:halt, {:ok, ret_type, {:call, method_name, typed_args, ret_type}}}

        true ->
          case Map.get(instances, {class_name, instance_key}) do
            # Virtual instance (inside constrained instance body) → constraint_call
            {:virtual, idx, _methods} ->
              {:cont, {:ok, [{:constraint_call, idx, method_name, typed_args, ret_type} | acc]}}

            # Constrained instance at call site → resolve sub-constraints
            {:constrained, inst_type_params, inst_constraints, _methods} ->
              resolved = resolve_constrained_instance(
                class_name, instance_key, inst_type_params, inst_constraints,
                concrete_type, typed_args, env
              )
              case resolved do
                {:ok, resolved_constraints} ->
                  {:cont, {:ok, [{:class_call, class_name, method_name, instance_key, typed_args, ret_type, resolved_constraints} | acc]}}
                {:error, _} = err ->
                  {:halt, err}
              end

            # Regular (unconstrained) instance
            methods when is_map(methods) ->
              {:cont, {:ok, [{:class_call, class_name, method_name, instance_key, typed_args, ret_type} | acc]}}

            nil ->
              {:halt, {:error, Error.new("no instance of `#{class_name}` for type `#{Vaisto.TypeSystem.Core.format_type(concrete_type)}`")}}
          end
      end
    end)
    |> case do
      {:ok, []} ->
        {:ok, ret_type, {:call, method_name, typed_args, ret_type}}
      {:ok, [single]} ->
        {:ok, ret_type, single}
      {:ok, multiple} ->
        # Multiple constraints resolved — use the last one (which is the primary class dispatch)
        # since all others are validation-only constraints
        {:ok, ret_type, List.last(multiple)}
      {:ok, ret_type, ast} ->
        {:ok, ret_type, ast}
      {:error, _} = err ->
        err
    end
  end

  # Resolve sub-constraints for a constrained instance at a call site
  # E.g., (show (Just 42)) → constrained Show Maybe needs Show :int
  defp resolve_constrained_instance(class_name, instance_key, inst_type_params, inst_constraints, concrete_type, typed_args, env) do
    instances = Map.get(env, :__instances__, %{})

    # Get the parameterized type to extract bindings
    resolved_param = resolve_instance_type(instance_key, env)
    bindings = extract_type_param_bindings(resolved_param, concrete_type, inst_type_params, typed_args)

    # Resolve each constraint with the bindings
    resolved = Enum.reduce_while(inst_constraints, {:ok, []}, fn {c_class, c_tvar}, {:ok, acc} ->
      bound_type = Map.get(bindings, c_tvar)
      cond do
        # Binding resolved to a concrete type — check instance exists
        bound_type != nil ->
          bound_key = normalize_instance_type(bound_type)
          case Map.get(instances, {c_class, bound_key}) do
            # Virtual instance (inside constrained instance body) — forward constraint ref
            {:virtual, vidx, _} ->
              {:cont, {:ok, acc ++ [{:constraint_ref, vidx}]}}

            nil ->
              {:halt, {:error, Error.new(
                "no instance of `#{c_class}` for type `#{Vaisto.TypeSystem.Core.format_type(bound_type)}`\n" <>
                "  required by constrained instance `#{class_name} #{instance_key}`"
              )}}

            # Sub-instance is itself constrained — recurse to resolve its constraints
            {:constrained, sub_tp, sub_cs, _sub_methods} ->
              inner_typed_args = extract_inner_typed_args(typed_args)
              case resolve_chained_constraint(c_class, bound_key, sub_tp, sub_cs, bound_type, inner_typed_args, env, 1) do
                {:ok, sub_resolved} ->
                  {:cont, {:ok, acc ++ [{:constrained_ref, c_class, bound_key, sub_resolved}]}}
                {:error, _} = err ->
                  {:halt, err}
              end

            _ ->
              {:cont, {:ok, acc ++ [{c_class, bound_key}]}}
          end

        # Binding unresolvable (e.g., nullary constructor of parametric type)
        # Use :any as placeholder — the constraint dict won't be invoked at runtime
        true ->
          {:cont, {:ok, acc ++ [{c_class, :any}]}}
      end
    end)

    resolved
  end

  # Recursively resolve sub-constraints for chained constrained instances.
  # E.g., Show (Maybe a) needs Show a; if a=Maybe Int, Show Maybe is itself
  # constrained → recurse to resolve Show Int.
  # Uses inner typed_args (peeled from the parent's typed_args) to extract bindings.
  defp resolve_chained_constraint(_c, _k, _tp, _cs, _concrete, _typed_args, _env, depth) when depth > 10 do
    {:error, Error.new("constraint resolution depth exceeded (possible infinite chain)")}
  end

  defp resolve_chained_constraint(c_class, bound_key, inst_type_params, inst_constraints,
                                   concrete_type, typed_args, env, depth) do
    instances = Map.get(env, :__instances__, %{})
    resolved_param = resolve_instance_type(bound_key, env)
    bindings = extract_type_param_bindings(resolved_param, concrete_type, inst_type_params, typed_args)

    Enum.reduce_while(inst_constraints, {:ok, []}, fn {sub_class, sub_tvar}, {:ok, acc} ->
      sub_bound = Map.get(bindings, sub_tvar)
      cond do
        sub_bound == nil ->
          {:cont, {:ok, acc ++ [{sub_class, :any}]}}
        true ->
          sub_key = normalize_instance_type(sub_bound)
          case Map.get(instances, {sub_class, sub_key}) do
            {:virtual, vidx, _} ->
              {:cont, {:ok, acc ++ [{:constraint_ref, vidx}]}}
            {:constrained, sub_tp, sub_cs, _} ->
              inner = extract_inner_typed_args(typed_args)
              case resolve_chained_constraint(sub_class, sub_key, sub_tp, sub_cs, sub_bound, inner, env, depth + 1) do
                {:ok, sub_constraints} ->
                  {:cont, {:ok, acc ++ [{:constrained_ref, sub_class, sub_key, sub_constraints}]}}
                {:error, _} = err ->
                  {:halt, err}
              end
            nil ->
              {:halt, {:error, Error.new(
                "no instance of `#{sub_class}` for type `#{Vaisto.TypeSystem.Core.format_type(sub_bound)}`\n" <>
                "  required by constrained instance `#{c_class} #{bound_key}`"
              )}}
            _ ->
              {:cont, {:ok, acc ++ [{sub_class, sub_key}]}}
          end
      end
    end)
  end

  # Extract inner typed args by peeling one constructor layer.
  # E.g., [{:call, :Just, [inner_arg], _type}] → [inner_arg]
  defp extract_inner_typed_args([{:call, _ctor, inner_args, _type} | _]), do: inner_args
  defp extract_inner_typed_args(_), do: []

  # Extract type param bindings by looking at the typed arguments of the call.
  # For `(show (Just 42))`, the typed_arg is `{:call, :Just, [{:lit, :int, 42}], sum_type}`.
  # Extract type param bindings by looking at the typed arguments of the call.
  # For `(show (Just 42))`, the typed_arg is `{:call, :Just, [{:lit, :int, 42}], sum_type}`.
  # We match only the relevant constructor variant to avoid index collisions.
  defp extract_type_param_bindings({:sum, _, p_variants}, _concrete_type, type_params, typed_args) do
    tvar_ids = p_variants
      |> Enum.flat_map(fn {_, types} -> collect_tvar_ids(types) end)
      |> Enum.uniq()
    tvar_to_param = Enum.zip(tvar_ids, type_params) |> Map.new()

    # Extract the constructor name and inner types from the first typed arg
    {ctor_name, inner_types} = extract_inner_types_from_args(typed_args)

    # Find the matching variant by constructor name; fall back to all variants
    target_variants = case ctor_name do
      nil -> p_variants
      name -> case Enum.find(p_variants, fn {n, _} -> n == name end) do
        nil -> p_variants
        variant -> [variant]
      end
    end

    target_variants
    |> Enum.flat_map(fn {_ctor, field_types} -> Enum.with_index(field_types) end)
    |> Enum.reduce(%{}, fn {field_type, idx}, acc ->
      case field_type do
        {:tvar, id} ->
          case {Map.get(tvar_to_param, id), Map.get(inner_types, idx)} do
            {nil, _} -> acc
            {_, nil} -> acc
            {param, concrete} -> Map.put(acc, param, concrete)
          end
        _ -> acc
      end
    end)
  end
  defp extract_type_param_bindings(_param, _concrete, _type_params, _typed_args), do: %{}

  # Extract constructor name and inner types from typed constructor args
  # (Just 42) → {:Just, %{0 => :int}}
  # (Nothing) → {:Nothing, %{}}
  # variable x : Maybe Int → {nil, bindings extracted from type}
  defp extract_inner_types_from_args(typed_args) do
    case typed_args do
      [{:call, ctor, ctor_args, _type} | _] ->
        inner = ctor_args
          |> Enum.with_index()
          |> Map.new(fn {arg, idx} -> {idx, typed_ast_type(arg)} end)
        {ctor, inner}
      [{:lit, :atom, ctor} | _] ->
        # Nullary constructor like (Nothing)
        {ctor, %{}}
      [{:var, _, {:sum, _, c_variants}} | _] ->
        # Variable with known sum type — extract from first variant with fields
        case Enum.find(c_variants, fn {_, fields} -> fields != [] end) do
          {_ctor, fields} ->
            inner = fields
              |> Enum.with_index()
              |> Map.new(fn {type, idx} -> {idx, type} end)
            {nil, inner}
          nil -> {nil, %{}}
        end
      _ -> {nil, %{}}
    end
  end

  # Extract the type from a typed AST node
  defp typed_ast_type({:lit, type, _}), do: type
  defp typed_ast_type({:var, _, type}), do: type
  defp typed_ast_type({:call, _, _, type}), do: type
  defp typed_ast_type({:class_call, _, _, _, _, type}), do: type
  defp typed_ast_type({:class_call, _, _, _, _, type, _}), do: type
  defp typed_ast_type({:if, _, _, _, type}), do: type
  defp typed_ast_type({:match, _, _, type}), do: type
  defp typed_ast_type({:let, _, _, type}), do: type
  defp typed_ast_type({:do, _, type}), do: type
  defp typed_ast_type({:list, _, type}), do: type
  defp typed_ast_type({:fn, _, _, type}), do: type
  defp typed_ast_type({:apply, _, _, type}), do: type
  defp typed_ast_type({:fn_ref, _, _, type}), do: type
  defp typed_ast_type(_), do: :any

  # ============================================================================
  # Apply substitution to typed AST — resolves all tvars in the output
  # ============================================================================

  alias Vaisto.TypeSystem.Core, as: C

  defp apply_subst_to_ast(subst, ast) when subst == %{}, do: ast

  # Literals — no type to resolve
  defp apply_subst_to_ast(_subst, {:lit, _, _} = lit), do: lit

  # Variables and references
  defp apply_subst_to_ast(subst, {:var, name, type}),
    do: {:var, name, C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:fn_ref, name, arity, type}),
    do: {:fn_ref, name, arity, C.apply_subst(subst, type)}

  # Collections
  defp apply_subst_to_ast(subst, {:list, elems, type}),
    do: {:list, apply_subst_to_asts(subst, elems), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:cons, head, tail, type}),
    do: {:cons, apply_subst_to_ast(subst, head), apply_subst_to_ast(subst, tail), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:tuple, elems, type}),
    do: {:tuple, apply_subst_to_asts(subst, elems), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:map, pairs, type}),
    do: {:map, Enum.map(pairs, fn {k, v} -> {apply_subst_to_ast(subst, k), apply_subst_to_ast(subst, v)} end), C.apply_subst(subst, type)}

  # Control flow
  defp apply_subst_to_ast(subst, {:if, cond_ast, then_ast, else_ast, type}),
    do: {:if, apply_subst_to_ast(subst, cond_ast), apply_subst_to_ast(subst, then_ast), apply_subst_to_ast(subst, else_ast), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:match, expr, clauses, type}),
    do: {:match, apply_subst_to_ast(subst, expr), apply_subst_to_clauses(subst, clauses), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:receive, clauses, type}),
    do: {:receive, apply_subst_to_clauses(subst, clauses), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:do, exprs, type}),
    do: {:do, apply_subst_to_asts(subst, exprs), C.apply_subst(subst, type)}

  # Bindings
  defp apply_subst_to_ast(subst, {:let, bindings, body, type}),
    do: {:let, bindings, apply_subst_to_ast(subst, body), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:fn, params, body, type}),
    do: {:fn, params, apply_subst_to_ast(subst, body), C.apply_subst(subst, type)}

  # Calls
  defp apply_subst_to_ast(subst, {:call, func, args, type}),
    do: {:call, func, apply_subst_to_asts(subst, args), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:apply, func_ast, args, type}),
    do: {:apply, apply_subst_to_ast(subst, func_ast), apply_subst_to_asts(subst, args), C.apply_subst(subst, type)}

  # Definitions
  defp apply_subst_to_ast(subst, {:defn, name, params, body, type}),
    do: {:defn, name, params, apply_subst_to_ast(subst, body), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:defn_multi, name, arity, clauses, type}),
    do: {:defn_multi, name, arity, apply_subst_to_clauses(subst, clauses), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:defval, name, value, type}),
    do: {:defval, name, apply_subst_to_ast(subst, value), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(_subst, {:deftype, _, _, _} = node), do: node

  # Process/concurrency
  defp apply_subst_to_ast(subst, {:process, name, init, handlers, type}),
    do: {:process, name, apply_subst_to_ast(subst, init), apply_subst_to_clauses(subst, handlers), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:supervise, strategy, children, type}),
    do: {:supervise, strategy, apply_subst_to_asts(subst, children), C.apply_subst(subst, type)}

  # Field access
  defp apply_subst_to_ast(subst, {:field_access, expr, field, type}),
    do: {:field_access, apply_subst_to_ast(subst, expr), field, C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:field_access, expr, field, type, constraint}),
    do: {:field_access, apply_subst_to_ast(subst, expr), field, C.apply_subst(subst, type), C.apply_subst(subst, constraint)}

  # Declarations — pass through
  defp apply_subst_to_ast(_subst, {:extern, _, _, _} = node), do: node
  defp apply_subst_to_ast(_subst, {:ns, _} = node), do: node
  defp apply_subst_to_ast(_subst, {:import, _, _} = node), do: node

  # Type class nodes
  defp apply_subst_to_ast(subst, {:class_call, class, method, inst, args, type}),
    do: {:class_call, class, method, inst, apply_subst_to_asts(subst, args), C.apply_subst(subst, type)}
  defp apply_subst_to_ast(subst, {:class_call, class, method, inst, args, type, constraints}),
    do: {:class_call, class, method, inst, apply_subst_to_asts(subst, args), C.apply_subst(subst, type), constraints}
  defp apply_subst_to_ast(subst, {:constraint_call, idx, method, args, type}),
    do: {:constraint_call, idx, method, apply_subst_to_asts(subst, args), C.apply_subst(subst, type)}

  # Instance nodes
  defp apply_subst_to_ast(_subst, {:instance, _, _, _, _} = node), do: node
  defp apply_subst_to_ast(_subst, {:instance_constrained, _, _, _, _, _, _} = node), do: node
  defp apply_subst_to_ast(_subst, {:defclass, _, _, _, _} = node), do: node

  # Module container
  defp apply_subst_to_ast(subst, {:module, forms}),
    do: {:module, apply_subst_to_asts(subst, forms)}

  # Catch-all — pass through unchanged (patterns, atoms, etc.)
  defp apply_subst_to_ast(_subst, other), do: other

  defp apply_subst_to_asts(subst, asts) when is_list(asts),
    do: Enum.map(asts, &apply_subst_to_ast(subst, &1))

  defp apply_subst_to_clauses(subst, clauses) when is_list(clauses) do
    Enum.map(clauses, fn
      {pattern, body, type} ->
        {pattern, apply_subst_to_ast(subst, body), C.apply_subst(subst, type)}
      other -> other
    end)
  end

  # Resolve the constraint type from argument types
  # If the constraint is {:tvar, N}, look at actual arg types
  defp resolve_constraint_type({:tvar, _}, [first_arg_type | _]) do
    first_arg_type
  end
  defp resolve_constraint_type(concrete, _arg_types), do: concrete

  # Outward normalization: full type → name atom (for registry lookup, emitter dict names)
  defp normalize_instance_type({:sum, name, _}), do: name
  defp normalize_instance_type({:record, name, _}), do: name
  defp normalize_instance_type(other), do: other

  # Inward normalization: name atom → full type (for type-checking instance method bodies)
  defp resolve_instance_type(for_type, env) when is_atom(for_type) do
    case Map.get(env, for_type) do
      {:sum, _, _} = sum -> instantiate_sum_tvars(sum)
      {:record, _, _} = rec -> rec
      _ -> for_type
    end
  end

  defp lookup_function(name, env) do
    case Map.get(env, name) do
      nil -> {:error, Errors.unknown_function(name)}
      {:fn, _, {:sum, _, _}} = ctor_type -> {:ok, instantiate_constructor_type(ctor_type)}
      {:fn, _, {:record, _, _}} = ctor_type -> {:ok, instantiate_constructor_type(ctor_type)}
      {:forall, vars, {:constrained, constraints, fn_type}} ->
        {:ok, {:constrained_method, vars, constraints, fn_type, name}}
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

  # Resolve bare type name atoms (e.g., :Maybe, :Point) to their full form from env
  defp extract_pattern_bindings({:call, _, _} = pattern, type_name, env)
       when is_atom(type_name) and type_name not in [:any, :int, :float, :string, :bool, :atom, :unit] do
    case Map.get(env, type_name) do
      {:sum, _, _} = sum_type -> extract_pattern_bindings(pattern, sum_type, env)
      {:record, _, _} = rec_type -> extract_pattern_bindings(pattern, rec_type, env)
      {:fn, _, {:sum, _, _} = sum_type} -> extract_pattern_bindings(pattern, sum_type, env)
      {:fn, _, {:record, _, _} = rec_type} -> extract_pattern_bindings(pattern, rec_type, env)
      _ -> extract_pattern_bindings(pattern, :any, env)
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

  # Exhaustiveness checking for match expressions
  defp check_exhaustiveness(clauses, {:sum, type_name, variants}) do
    covered = clauses
      |> Enum.map(fn {pattern, _body} -> extract_variant_name(pattern) end)
      |> Enum.reject(&is_nil/1)
      |> MapSet.new()

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

  defp check_exhaustiveness(clauses, :any) do
    has_catch_all = Enum.any?(clauses, fn {pattern, _body} ->
      is_catch_all_pattern?(pattern)
    end)

    if has_catch_all do
      :ok
    else
      tags = clauses
        |> Enum.map(fn {pattern, _body} -> extract_tuple_tag(pattern) end)
        |> Enum.filter(&(&1 != nil))
        |> MapSet.new()

      result_tags = MapSet.new([:ok, :error])
      has_result_tags = not MapSet.disjoint?(tags, result_tags)
      missing_result_tags = MapSet.difference(result_tags, tags)

      if has_result_tags and MapSet.size(missing_result_tags) > 0 do
        {:error, Errors.non_exhaustive_result(MapSet.to_list(missing_result_tags))}
      else
        :ok
      end
    end
  end

  defp check_exhaustiveness(_clauses, _expr_type), do: :ok

  defp extract_variant_name({:call, name, _, _}), do: name
  defp extract_variant_name({:pattern, name, _, _}), do: name
  defp extract_variant_name(_), do: nil

  defp extract_tuple_tag({:tuple, [{:lit, :atom, tag} | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple, [{:atom, tag} | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple, [tag | _], _}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [{:lit, :atom, tag} | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [{:atom, tag} | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag({:tuple_pattern, [tag | _]}) when is_atom(tag), do: tag
  defp extract_tuple_tag(_), do: nil

  defp is_catch_all_pattern?(:_), do: true
  defp is_catch_all_pattern?(name) when is_atom(name) and name != :_, do: true
  defp is_catch_all_pattern?({:var, _, _}), do: true
  defp is_catch_all_pattern?(_), do: false

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

  # Resolve bare type name atoms (e.g., :Maybe, :Point) to their full form from env
  defp type_pattern({:call, _, _} = pattern, type_name, env)
       when is_atom(type_name) and type_name not in [:any, :int, :float, :string, :bool, :atom, :unit] do
    case Map.get(env, type_name) do
      {:sum, _, _} = sum_type -> type_pattern(pattern, sum_type, env)
      {:record, _, _} = rec_type -> type_pattern(pattern, rec_type, env)
      {:fn, _, {:sum, _, _} = sum_type} -> type_pattern(pattern, sum_type, env)
      {:fn, _, {:record, _, _} = rec_type} -> type_pattern(pattern, rec_type, env)
      _ -> type_pattern(pattern, :any, env)
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


  # Context-threaded map literal checking
  defp check_map_literal_s(pairs, ctx) do
    case check_map_pairs_s(pairs, ctx, [], []) do
      {:ok, field_types, typed_pairs, ctx} ->
        map_type = {:row, field_types, :closed}
        {:ok, map_type, {:map, typed_pairs, map_type}, ctx}
      error -> error
    end
  end

  defp check_map_pairs_s([], ctx, field_types, typed_pairs) do
    {:ok, Enum.reverse(field_types), Enum.reverse(typed_pairs), ctx}
  end
  defp check_map_pairs_s([{key_expr, val_expr} | rest], ctx, field_types, typed_pairs) do
    with {:ok, _key_type, typed_key, ctx} <- check_s(key_expr, ctx),
         {:ok, val_type, typed_val, ctx} <- check_s(val_expr, ctx) do
      field_name = case typed_key do
        {:lit, :atom, a} -> a
        _ -> :dynamic
      end
      check_map_pairs_s(rest, ctx,
        [{field_name, val_type} | field_types],
        [{typed_key, typed_val} | typed_pairs])
    end
  end

  # Context-threaded check_args: folds left-to-right, threading substitution
  defp check_args_s(args, ctx) do
    Enum.reduce_while(args, {:ok, [], [], ctx}, fn arg, {:ok, types, typed, ctx} ->
      case check_s(arg, ctx) do
        {:ok, type, ast, ctx} -> {:cont, {:ok, [type | types], [ast | typed], ctx}}
        {:error, _} = err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, types, typed, ctx} -> {:ok, Enum.reverse(types), Enum.reverse(typed), ctx}
      {:error, _} = err -> err
    end
  end

  # Context-threaded check_exprs_sequence: threads substitution through expression list
  defp check_exprs_sequence_s([], ctx), do: {:ok, [], :unit, ctx}
  defp check_exprs_sequence_s(exprs, ctx) do
    Enum.reduce_while(exprs, {:ok, [], :unit, ctx}, fn expr, {:ok, typed, _last_type, ctx} ->
      case check_s(expr, ctx) do
        {:ok, type, ast, ctx} -> {:cont, {:ok, [ast | typed], type, ctx}}
        {:error, _} = err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, typed, last_type, ctx} -> {:ok, Enum.reverse(typed), last_type, ctx}
      {:error, _} = err -> err
    end
  end

  # Context-threaded check_bindings: threads substitution through let bindings
  defp check_bindings_s([], ctx, acc), do: {:ok, ctx, Enum.reverse(acc)}

  defp check_bindings_s([{name, expr} | rest], ctx, acc) when is_atom(name) do
    case check_s(expr, ctx) do
      {:ok, type, typed_expr, ctx} ->
        extended_ctx = %{ctx | env: ctx.env |> Map.put(name, type) |> add_local_var(name)}
        check_bindings_s(rest, extended_ctx, [{name, typed_expr, type} | acc])
      error -> error
    end
  end

  defp check_bindings_s([{{:tuple_pattern, elements}, expr} | rest], ctx, acc) do
    case check_s(expr, ctx) do
      {:ok, _type, typed_expr, ctx} ->
        bindings = extract_pattern_bindings({:tuple_pattern, elements}, :any, ctx.env)
        extended_ctx = %{ctx | env: Enum.reduce(bindings, ctx.env, fn {var, var_type}, e ->
          Map.put(e, var, var_type)
        end)}
        typed_pattern = type_pattern({:tuple_pattern, elements}, :any, ctx.env)
        check_bindings_s(rest, extended_ctx, [{typed_pattern, typed_expr, :any} | acc])
      error -> error
    end
  end

  defp check_bindings_s([{{:bracket, {:cons, head, tail}}, expr} | rest], ctx, acc) do
    case check_s(expr, ctx) do
      {:ok, {:list, elem_type}, typed_expr, ctx} ->
        extended_ctx = %{ctx | env: ctx.env
          |> Map.put(head, elem_type) |> add_local_var(head)
          |> Map.put(tail, {:list, elem_type}) |> add_local_var(tail)}
        typed_pattern = {:cons_pattern, head, tail, {:list, elem_type}}
        check_bindings_s(rest, extended_ctx, [{typed_pattern, typed_expr, {:list, elem_type}} | acc])
      {:ok, type, typed_expr, ctx} ->
        extended_ctx = %{ctx | env: ctx.env
          |> Map.put(head, :any) |> add_local_var(head)
          |> Map.put(tail, {:list, :any}) |> add_local_var(tail)}
        typed_pattern = {:cons_pattern, head, tail, type}
        check_bindings_s(rest, extended_ctx, [{typed_pattern, typed_expr, type} | acc])
      error -> error
    end
  end

  # Context-threaded check_match_clauses
  defp check_match_clauses_s(clauses, expr_type, ctx) do
    results = Enum.map(clauses, fn {pattern, body} ->
      check_match_clause_s(pattern, body, expr_type, ctx)
    end)

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        typed_clauses = Enum.map(results, fn {:ok, clause, _ctx} -> clause end)
        # Thread ctx from last clause (accumulates all branch substitutions)
        last_ctx = results |> List.last() |> elem(2)
        [{_pattern, _body, first_type} | rest_clauses] = typed_clauses

        unified_result = Enum.reduce_while(rest_clauses, {:ok, first_type, last_ctx}, fn {_, _, body_type}, {:ok, acc_type, ctx} ->
          case unify_types_s(acc_type, body_type, ctx) do
            {:ok, unified, ctx} -> {:cont, {:ok, unified, ctx}}
            {:error, _} = err -> {:halt, err}
          end
        end)

        case unified_result do
          {:error, _} = err -> err
          {:ok, result_type, ctx} ->
            case check_exhaustiveness(clauses, expr_type) do
              :ok -> {:ok, result_type, typed_clauses, ctx}
              {:error, _} = err -> err
            end
        end
    end
  end

  defp check_match_clause_s(pattern, body, expr_type, ctx) do
    instantiated_type = instantiate_sum_tvars(expr_type)
    bindings = extract_pattern_bindings(pattern, instantiated_type, ctx.env)
    extended_ctx = %{ctx | env: Enum.reduce(bindings, ctx.env, fn {name, type}, acc ->
      Map.put(acc, name, type)
    end)}
    typed_pattern = type_pattern(pattern, instantiated_type, ctx.env)

    case check_s(body, extended_ctx) do
      {:ok, body_type, typed_body, ctx} ->
        {:ok, {typed_pattern, typed_body, body_type}, ctx}
      error -> error
    end
  end

  # Context-threaded check_receive_clauses
  defp check_receive_clauses_s(clauses, ctx) do
    results = Enum.map(clauses, fn {pattern, body} ->
      check_receive_clause_s(pattern, body, ctx)
    end)

    case Enum.find(results, &match?({:error, _}, &1)) do
      {:error, _} = err -> err
      nil ->
        typed_clauses = Enum.map(results, fn {:ok, clause, _ctx} -> clause end)
        last_ctx = results |> List.last() |> elem(2)
        [{_pattern, _body, result_type} | _] = typed_clauses
        {:ok, result_type, typed_clauses, last_ctx}
    end
  end

  defp check_receive_clause_s(pattern, body, ctx) do
    bindings = extract_pattern_bindings(pattern, :any, ctx.env)
    extended_ctx = %{ctx | env: Enum.reduce(bindings, ctx.env, fn {name, type}, acc ->
      Map.put(acc, name, type)
    end)}
    typed_pattern = type_pattern(pattern, :any, ctx.env)

    case check_s(body, extended_ctx) do
      {:ok, body_type, typed_body, ctx} ->
        {:ok, {typed_pattern, typed_body, body_type}, ctx}
      error -> error
    end
  end

  # Context-threaded check_handlers
  defp check_handlers_s(handlers, state_type, ctx) do
    handler_ctx = %{ctx | env: Map.put(ctx.env, :state, state_type)}

    Enum.reduce_while(handlers, {:ok, [], handler_ctx}, fn {msg, body}, {:ok, acc, ctx} ->
      case check_s(body, ctx) do
        {:ok, ret_type, typed_body, ctx} -> {:cont, {:ok, [{msg, typed_body, ret_type} | acc], ctx}}
        {:error, _} = err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, typed, ctx} -> {:ok, Enum.reverse(typed), ctx}
      {:error, _} = err -> err
    end
  end

  # Context-threaded check_generic_call
  defp check_generic_call_s(func, args, ctx) do
    with {:ok, func_type} <- lookup_function(func, ctx.env),
         {:ok, arg_types, typed_args, ctx} <- check_args_s(args, ctx),
         {:ok, ret_type, ctx} <- unify_call_s(func_type, arg_types, ctx) do
      {:ok, ret_type, {:call, func, typed_args, ret_type}, ctx}
    end
  end

  # Context-threaded check_class_method_call
  defp check_class_method_call_s(method_name, vars, constraints, fn_type, args, ctx) do
    base_id = :erlang.unique_integer([:positive, :monotonic]) * 1000
    fresh_subst = vars
      |> Enum.with_index()
      |> Map.new(fn {var, idx} -> {var, {:tvar, base_id + idx}} end)

    inst_fn_type = Vaisto.TypeSystem.Core.apply_subst(fresh_subst, fn_type)
    inst_constraints = Enum.map(constraints, fn {class, t} ->
      {class, Vaisto.TypeSystem.Core.apply_subst(fresh_subst, t)}
    end)

    with {:ok, arg_types, typed_args, ctx} <- check_args_s(args, ctx),
         {:ok, ret_type, ctx} <- unify_call_s(inst_fn_type, arg_types, ctx, args) do
      # Resolve constraints using the old helper (stateless resolution)
      case resolve_class_constraints(inst_constraints, method_name, ret_type, typed_args, arg_types, ctx.env) do
        {:ok, ret_type, ast} -> {:ok, ret_type, ast, ctx}
        {:error, _} = err -> err
      end
    end
  end

  # Context-threaded HOF builtins

  defp check_builtin_map_s(func_name, list_expr, ctx) do
    with {:ok, func_type} <- lookup_function(func_name, ctx.env),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], ret_type}, {:list, _elem_type}} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [func_name, typed_list], result_type}, ctx}
        {{:fn, [_], ret_type}, :any} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [func_name, typed_list], result_type}, ctx}
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

  defp check_builtin_map_anon_s(fn_expr, list_expr, ctx) do
    with {:ok, func_type, typed_fn, ctx} <- check_s(fn_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], ret_type}, {:list, _elem_type}} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [typed_fn, typed_list], result_type}, ctx}
        {{:fn, [_], ret_type}, :any} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [typed_fn, typed_list], result_type}, ctx}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, Errors.mapper_arity(:map, 1, length(args))}
        {_, other} ->
          {:error, Errors.not_a_list(:map, other)}
      end
    end
  end

  defp check_builtin_filter_s(func_name, list_expr, ctx) do
    with {:ok, func_type} <- lookup_function(func_name, ctx.env),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], :bool}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [func_name, typed_list], {:list, elem_type}}, ctx}
        {{:fn, [_arg_type], :any}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [func_name, typed_list], {:list, elem_type}}, ctx}
        {{:fn, [_], :bool}, :any} ->
          {:ok, {:list, :any}, {:call, :filter, [func_name, typed_list], {:list, :any}}, ctx}
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

  defp check_builtin_filter_anon_s(fn_expr, list_expr, ctx) do
    with {:ok, func_type, typed_fn, ctx} <- check_s(fn_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_arg_type], :bool}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [typed_fn, typed_list], {:list, elem_type}}, ctx}
        {{:fn, [_arg_type], :any}, {:list, elem_type}} ->
          {:ok, list_type, {:call, :filter, [typed_fn, typed_list], {:list, elem_type}}, ctx}
        {{:fn, [_], :bool}, :any} ->
          {:ok, {:list, :any}, {:call, :filter, [typed_fn, typed_list], {:list, :any}}, ctx}
        {{:fn, [_], ret_type}, _} when ret_type not in [:bool, :any] ->
          {:error, Errors.predicate_not_bool(ret_type)}
        {{:fn, args, _}, _} when length(args) != 1 ->
          {:error, Errors.mapper_arity(:filter, 1, length(args))}
        {_, other} ->
          {:error, Errors.not_a_list(:filter, other)}
      end
    end
  end

  defp check_builtin_fold_s(func_name, init_expr, list_expr, ctx) do
    with {:ok, func_type} <- lookup_function(func_name, ctx.env),
         {:ok, init_type, typed_init, ctx} <- check_s(init_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_acc_type, _elem_type], ret_type}, {:list, _}} ->
          {:ok, ret_type, {:call, :fold, [func_name, typed_init, typed_list], ret_type}, ctx}
        {{:fn, [_, _], ret_type}, :any} ->
          {:ok, ret_type, {:call, :fold, [func_name, typed_init, typed_list], ret_type}, ctx}
        {{:fn, args, _}, _} when length(args) != 2 ->
          {:error, Errors.mapper_arity(:fold, 2, length(args))}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}, ctx}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [func_name, typed_init, typed_list], init_type}, ctx}
        {_, other} ->
          {:error, Errors.not_a_list(:fold, other)}
      end
    end
  end

  defp check_builtin_fold_anon_s(fn_expr, init_expr, list_expr, ctx) do
    with {:ok, func_type, typed_fn, ctx} <- check_s(fn_expr, ctx),
         {:ok, init_type, typed_init, ctx} <- check_s(init_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- check_s(list_expr, ctx) do
      case {func_type, list_type} do
        {{:fn, [_acc_type, _elem_type], ret_type}, {:list, _}} ->
          {:ok, ret_type, {:call, :fold, [typed_fn, typed_init, typed_list], ret_type}, ctx}
        {{:fn, [_, _], ret_type}, :any} ->
          {:ok, ret_type, {:call, :fold, [typed_fn, typed_init, typed_list], ret_type}, ctx}
        {{:fn, args, _}, _} when length(args) != 2 ->
          {:error, Errors.mapper_arity(:fold, 2, length(args))}
        {_, {:list, _}} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}, ctx}
        {_, :any} ->
          {:ok, init_type, {:call, :fold, [typed_fn, typed_init, typed_list], init_type}, ctx}
        {_, other} ->
          {:error, Errors.not_a_list(:fold, other)}
      end
    end
  end


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

  # Stateless compatibility check using real unification (no substitution threaded).
  defp types_unifiable?(t1, t2) do
    match?({:ok, _, _}, Vaisto.TypeSystem.Unify.unify(t1, t2))
  end

  @numeric_types [:int, :float, :num]
  defp numeric_compatible?(t1, t2), do: t1 in @numeric_types and t2 in @numeric_types

  # Context-threaded versions — accumulate unification substitutions into ctx

  defp unify_types_s(t1, t2, ctx) do
    # Apply current substitution first to resolve any already-bound tvars
    resolved_t1 = Vaisto.TypeSystem.Core.apply_subst(ctx.subst, t1)
    resolved_t2 = Vaisto.TypeSystem.Core.apply_subst(ctx.subst, t2)

    case Vaisto.TypeSystem.Unify.unify(resolved_t1, resolved_t2, ctx.subst) do
      {:ok, new_subst, _} ->
        unified = Vaisto.TypeSystem.Core.apply_subst(new_subst, resolved_t1)
        {:ok, unified, %{ctx | subst: new_subst}}
      {:error, _} ->
        if numeric_compatible?(resolved_t1, resolved_t2),
          do: {:ok, :num, ctx},
          else: {:error, Errors.branch_type_mismatch(resolved_t1, resolved_t2)}
    end
  end

  defp expect_same_type_s(t1, t2, ctx) do
    case unify_types_s(t1, t2, ctx) do
      {:ok, _unified, ctx} -> {:ok, ctx}
      {:error, _} = err -> err
    end
  end

  defp unify_call_s(func_type, actual_args, ctx, original_args \\ [])

  defp unify_call_s({:fn, expected_args, ret_type}, actual_args, ctx, original_args) do
    unify_call_poly_s({:fn, expected_args, ret_type}, actual_args, ctx, original_args)
  end

  defp unify_call_s(:any, _actual_args, ctx, _original_args) do
    {:ok, :any, ctx}
  end

  defp unify_call_poly_s({:fn, expected_args, ret_type}, actual_args, ctx, original_args) do
    if length(expected_args) != length(actual_args) do
      {:error, Errors.arity_mismatch(:function, length(expected_args), length(actual_args))}
    else
      padded_orig = original_args ++ List.duplicate(nil, length(actual_args))

      result = Enum.zip([expected_args, actual_args, padded_orig])
        |> Enum.with_index()
        |> Enum.reduce_while({:ok, ctx.subst}, fn {{exp, act, _orig}, idx}, {:ok, subst} ->
          resolved_exp = Vaisto.TypeSystem.Core.apply_subst(subst, exp)
          cond do
            resolved_exp == :any or act == :any ->
              case exp do
                {:tvar, id} -> {:cont, {:ok, Map.put_new(subst, id, :any)}}
                _ -> {:cont, {:ok, subst}}
              end

            true ->
              case Vaisto.TypeSystem.Unify.unify(resolved_exp, act, subst) do
                {:ok, new_subst, _} -> {:cont, {:ok, new_subst}}
                {:error, _} ->
                  if match?({:tvar, _}, exp) and numeric_compatible?(resolved_exp, act) do
                    {:tvar, id} = exp
                    {:cont, {:ok, Map.put(subst, id, :num)}}
                  else
                    {:halt, {:error, Errors.type_mismatch(resolved_exp, act, note: "at argument #{idx + 1}")}}
                  end
              end
          end
        end)

      case result do
        {:ok, subst} ->
          resolved_ret = Vaisto.TypeSystem.Core.apply_subst(subst, ret_type)
          {:ok, resolved_ret, %{ctx | subst: subst}}
        {:error, _} = err -> err
      end
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

        {:defclass, class_name, type_params, methods, %Vaisto.Parser.Loc{}} ->
          collect_defclass_signature(class_name, type_params, methods, acc_env)

        {:defclass, class_name, type_params, methods} ->
          collect_defclass_signature(class_name, type_params, methods, acc_env)

        {:instance, class_name, for_type, _methods, %Vaisto.Parser.Loc{}} ->
          collect_instance_signature(class_name, for_type, acc_env)

        {:instance, class_name, for_type, _methods} ->
          collect_instance_signature(class_name, for_type, acc_env)

        {:instance_constrained, class_name, type_name, type_params, constraints, _methods, %Vaisto.Parser.Loc{}} ->
          collect_constrained_instance_signature(class_name, type_name, type_params, constraints, acc_env)

        {:instance_constrained, class_name, type_name, type_params, constraints, _methods} ->
          collect_constrained_instance_signature(class_name, type_name, type_params, constraints, acc_env)

        {:deftype_deriving, name, type_def, classes, %Vaisto.Parser.Loc{}} ->
          expand_deriving_signatures(name, type_def, classes, acc_env)

        {:deftype_deriving, name, type_def, classes} ->
          expand_deriving_signatures(name, type_def, classes, acc_env)

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

  defp collect_defclass_signature(class_name, type_params, methods, env) do
    # Create type variable mapping: param_name -> {:tvar, index}
    param_map = type_params
      |> Enum.with_index()
      |> Map.new(fn {param, idx} -> {param, {:tvar, idx}} end)

    tvar_ids = Enum.with_index(type_params) |> Enum.map(fn {_, idx} -> idx end)

    # Parse method signatures and collect defaults
    # Methods are 4-tuples: {name, params, ret_type, body|nil}
    parsed_methods = Enum.map(methods, fn {method_name, params, ret_type, _body} ->
      param_types = Enum.map(params, fn {_name, type} ->
        Map.get(param_map, type, parse_type_expr(type))
      end)
      parsed_ret = Map.get(param_map, ret_type, parse_type_expr(ret_type))
      {method_name, {:fn, param_types, parsed_ret}}
    end)

    # Collect default implementations: method_name => {:default, param_names, body_ast}
    defaults = methods
      |> Enum.filter(fn {_name, _params, _ret_type, body} -> body != nil end)
      |> Map.new(fn {name, params, _ret_type, body} ->
        param_names = Enum.map(params, fn {pname, _type} -> pname end)
        {name, {:default, param_names, body}}
      end)

    # Store class definition with defaults (5-tuple)
    class_def = {:class, class_name, tvar_ids, parsed_methods, defaults}
    classes = Map.get(env, :__classes__, %{})
    env = Map.put(env, :__classes__, Map.put(classes, class_name, class_def))

    # Register each method as a constrained type in env
    Enum.reduce(parsed_methods, env, fn {method_name, method_type}, acc_env ->
      constraints = Enum.map(tvar_ids, fn id -> {class_name, {:tvar, id}} end)
      scheme = {:forall, tvar_ids, {:constrained, constraints, method_type}}
      Map.put(acc_env, method_name, scheme)
    end)
  end

  defp collect_instance_signature(class_name, for_type, env) do
    classes = Map.get(env, :__classes__, %{})
    class_def = Map.get(classes, class_name)
    case class_def do
      tuple when is_tuple(tuple) and elem(tuple, 0) == :class ->
        {tvar_ids, method_sigs, _defaults} = extract_class_parts(class_def)
        # Resolve ADT name atom to full type for substitution
        resolved_type = resolve_instance_type(for_type, env)
        subst = Map.new(tvar_ids, fn id -> {id, resolved_type} end)
        concrete_methods = Map.new(method_sigs, fn {method_name, method_type} ->
          {method_name, Vaisto.TypeSystem.Core.apply_subst(subst, method_type)}
        end)

        instances = Map.get(env, :__instances__, %{})
        Map.put(env, :__instances__, Map.put(instances, {class_name, for_type}, concrete_methods))

      nil ->
        # Class not found — will error during checking pass
        env
    end
  end

  defp collect_constrained_instance_signature(class_name, type_name, type_params, constraints, env) do
    classes = Map.get(env, :__classes__, %{})
    class_def = Map.get(classes, class_name)
    case class_def do
      tuple when is_tuple(tuple) and elem(tuple, 0) == :class ->
        {tvar_ids, method_sigs, _defaults} = extract_class_parts(class_def)
        # Resolve the ADT type and compute concrete methods
        resolved_type = resolve_instance_type(type_name, env)
        subst = Map.new(tvar_ids, fn id -> {id, resolved_type} end)
        concrete_methods = Map.new(method_sigs, fn {method_name, method_type} ->
          {method_name, Vaisto.TypeSystem.Core.apply_subst(subst, method_type)}
        end)

        # Store with constraint metadata for call-site resolution
        entry = {:constrained, type_params, constraints, concrete_methods}
        instances = Map.get(env, :__instances__, %{})
        Map.put(env, :__instances__, Map.put(instances, {class_name, type_name}, entry))

      nil ->
        env
    end
  end

  defp expand_deriving_signatures(name, type_def, classes, env) do
    env_with_type = collect_deftype_signature(name, type_def, env)
    Enum.reduce(classes, env_with_type, fn class_name, acc ->
      collect_instance_signature(class_name, name, acc)
    end)
  end

  defp check_module_forms([], _env, acc, errors) do
    case errors do
      [] -> {:ok, :module, {:module, Enum.reverse(acc)}}
      _ -> {:error, Enum.reverse(errors)}
    end
  end

  defp check_module_forms([{:deftype_deriving, name, type_def, classes, loc} | rest], env, acc, errors) do
    check_deftype_deriving(name, type_def, classes, loc, env, rest, acc, errors)
  end
  defp check_module_forms([{:deftype_deriving, name, type_def, classes} | rest], env, acc, errors) do
    check_deftype_deriving(name, type_def, classes, %Vaisto.Parser.Loc{}, env, rest, acc, errors)
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

  # Deriving: check the deftype, then synthesize and check each derived instance
  defp check_deftype_deriving(name, type_def, classes, loc, env, rest, acc, errors) do
    case check({:deftype, name, type_def, loc}, env) do
      {:ok, _type, typed_deftype} ->
        new_env = update_env_from_typed_form(typed_deftype, env)

        {instance_acc, err_acc} = Enum.reduce(classes, {[], errors}, fn class, {insts, errs} ->
          case synthesize_and_check_instance(class, name, type_def, loc, new_env) do
            {:ok, _type, typed_inst} -> {[typed_inst | insts], errs}
            {:error, err} -> {insts, [err | errs]}
          end
        end)

        new_acc = Enum.reverse(instance_acc) ++ [typed_deftype | acc]
        check_module_forms(rest, new_env, new_acc, err_acc)

      {:error, err} ->
        check_module_forms(rest, env, acc, [err | errors])
    end
  end

  # Eq: derivable for all types — uses BEAM structural equality
  defp synthesize_and_check_instance(:Eq, for_type, _type_def, loc, env) do
    methods = [{:eq, [:x, :y], {:call, :==, [:x, :y], loc}}]
    check({:instance, :Eq, for_type, methods, loc}, env)
  end

  # Show: derivable for enum-like sum types only (all variants have 0 fields)
  defp synthesize_and_check_instance(:Show, for_type, {:sum, variants}, loc, env) do
    has_fields? = Enum.any?(variants, fn {_ctor, fields} -> fields != [] end)

    if has_fields? do
      {:error, Error.new("cannot derive Show for `#{for_type}`: variants with fields require a manual instance")}
    else
      match_clauses = Enum.map(variants, fn {ctor_name, []} ->
        {{:call, ctor_name, [], loc}, {:string, Atom.to_string(ctor_name)}}
      end)
      match_expr = {:match, :x, match_clauses, loc}
      methods = [{:show, [:x], match_expr}]
      check({:instance, :Show, for_type, methods, loc}, env)
    end
  end

  defp synthesize_and_check_instance(:Show, for_type, {:product, _}, _loc, _env) do
    {:error, Error.new("cannot derive Show for record type `#{for_type}`")}
  end

  defp synthesize_and_check_instance(class, _for_type, _type_def, _loc, _env) do
    {:error, Error.new("cannot derive `#{class}`: only Eq and Show are derivable")}
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
    unified = join_types_list(pattern_types)
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

  # Least-upper-bound across a list of types — used for multi-clause function
  # parameter types. Never fails: incompatible types fall back to :any.
  defp join_types_list([]), do: :any
  defp join_types_list([t]), do: t
  defp join_types_list([t1 | rest]) do
    Enum.reduce(rest, t1, &join_types/2)
  end

  # Least-upper-bound of two types — never fails, falls back to :any.
  # Used for multi-clause parameter unification where we want the broadest
  # compatible type rather than an error.
  defp join_types(:any, t), do: t
  defp join_types(t, :any), do: t
  defp join_types(t, t), do: t
  defp join_types({:list, e1}, {:list, e2}), do: {:list, join_types(e1, e2)}
  defp join_types({:fn, a1, r1}, {:fn, a2, r2}) when length(a1) == length(a2) do
    unified_args = Enum.zip(a1, a2) |> Enum.map(fn {x, y} -> join_types(x, y) end)
    {:fn, unified_args, join_types(r1, r2)}
  end
  defp join_types({:record, n, f1}, {:record, n, f2}) do
    # Same record name, unify fields
    {:record, n, join_fields(f1, f2)}
  end
  defp join_types({:sum, n, v1}, {:sum, n, v2}) when v1 == v2 do
    {:sum, n, v1}
  end
  defp join_types(_, _), do: :any  # Incompatible types fall back to :any

  defp join_fields(f1, f2) do

    map2 = Map.new(f2)
    Enum.map(f1, fn {name, type1} ->
      type2 = Map.get(map2, name, :any)
      {name, join_types(type1, type2)}
    end)
  end

  # ============================================================================
  # Parametric Polymorphism — Fresh Instantiation
  # ============================================================================

  # Generate a fresh tvar id that won't collide with parser-assigned ids (0, 1, 2...)
  defp fresh_tvar_id do
    System.unique_integer([:positive, :monotonic]) + 10_000
  end

  # Instantiate a constructor type with fresh tvars so each call site is independent
  defp instantiate_constructor_type({:fn, params, ret}) do
    tvar_ids = collect_tvar_ids(params ++ [ret])
    if tvar_ids == [] do
      {:fn, params, ret}
    else
      mapping = Map.new(tvar_ids, fn id -> {id, {:tvar, fresh_tvar_id()}} end)
      {:fn,
       Enum.map(params, &Vaisto.TypeSystem.Core.apply_subst(mapping, &1)),
       Vaisto.TypeSystem.Core.apply_subst(mapping, ret)}
    end
  end

  # Instantiate a sum type with fresh tvars so each match clause is independent
  defp instantiate_sum_tvars({:sum, name, variants}) do
    tvar_ids = variants
    |> Enum.flat_map(fn {_, types} -> collect_tvar_ids(types) end)
    |> Enum.uniq()
    if tvar_ids == [] do
      {:sum, name, variants}
    else
      mapping = Map.new(tvar_ids, fn id -> {id, {:tvar, fresh_tvar_id()}} end)
      Vaisto.TypeSystem.Core.apply_subst(mapping, {:sum, name, variants})
    end
  end
  defp instantiate_sum_tvars(other), do: other

  # Collect all tvar ids from a type or list of types
  defp collect_tvar_ids(types) when is_list(types) do
    Enum.flat_map(types, &collect_tvar_ids/1) |> Enum.uniq()
  end
  defp collect_tvar_ids({:tvar, id}), do: [id]
  defp collect_tvar_ids({:fn, params, ret}), do: collect_tvar_ids(params) ++ collect_tvar_ids(ret)
  defp collect_tvar_ids({:sum, _, variants}) do
    Enum.flat_map(variants, fn {_, types} -> collect_tvar_ids(types) end)
  end
  defp collect_tvar_ids({:list, t}), do: collect_tvar_ids(t)
  defp collect_tvar_ids({:record, _, fields}) do
    Enum.flat_map(fields, fn {_, t} -> collect_tvar_ids(t) end)
  end
  defp collect_tvar_ids(_), do: []
end
