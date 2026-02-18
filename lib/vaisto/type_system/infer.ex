defmodule Vaisto.TypeSystem.Infer do
  @moduledoc """
  Hindley-Milner type inference for Vaisto.

  Uses Algorithm W to infer types for expressions without explicit annotations.
  Supports let-polymorphism for generic functions like identity.

  ## How It Works

  1. **Generate fresh type variables** for unknowns (function params, etc.)
  2. **Traverse the AST**, collecting type constraints
  3. **Unify constraints** to solve for type variables
  4. **Apply final substitution** to get concrete types

  ## Example

      (fn [x] x)  → infers {:fn, [t0], t0} (identity function)
      (fn [x] (+ x 1)) → infers {:fn, [:int], :int}
  """

  alias Vaisto.TypeSystem.Context
  alias Vaisto.TypeSystem.Core

  # Built-in operators with their types
  @primitives %{
    :+ => {:fn, [:int, :int], :int},
    :- => {:fn, [:int, :int], :int},
    :* => {:fn, [:int, :int], :int},
    :/ => {:fn, [:int, :int], :int},
    :== => {:forall, [0], {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}},
    :< => {:fn, [:int, :int], :bool},
    :> => {:fn, [:int, :int], :bool},
    :<= => {:fn, [:int, :int], :bool},
    :>= => {:fn, [:int, :int], :bool},
    :!= => {:forall, [0], {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}}
  }

  @doc "Returns the built-in primitives environment (for testing)."
  def __primitives__, do: @primitives

  @doc """
  Infer the type of an expression.

  Returns {:ok, type, typed_ast} or {:error, reason}.
  """
  def infer(expr, env \\ @primitives) do
    ctx = Context.new(env)

    case infer_expr(expr, ctx) do
      {:ok, type, typed_ast, ctx} ->
        # Apply final substitution to get concrete type
        final_type = Context.apply(ctx, type)
        final_ast = apply_subst_to_ast(typed_ast, ctx.subst)
        {:ok, final_type, final_ast}

      {:error, _} = err ->
        err
    end
  end

  # --- Literals ---

  defp infer_expr(n, ctx) when is_integer(n) do
    {:ok, :int, {:lit, :int, n}, ctx}
  end

  defp infer_expr(f, ctx) when is_float(f) do
    {:ok, :float, {:lit, :float, f}, ctx}
  end

  defp infer_expr(true, ctx), do: {:ok, :bool, {:lit, :bool, true}, ctx}
  defp infer_expr(false, ctx), do: {:ok, :bool, {:lit, :bool, false}, ctx}

  defp infer_expr({:string, s}, ctx) do
    {:ok, :string, {:lit, :string, s}, ctx}
  end

  defp infer_expr({:atom, a}, ctx) when is_atom(a) do
    {:ok, {:atom, a}, {:lit, :atom, a}, ctx}
  end

  # --- Variables ---

  defp infer_expr({:var, name}, ctx) do
    case Context.lookup(ctx, name) do
      {:ok, scheme} ->
        {type, ctx} = Context.instantiate(ctx, scheme)
        {:ok, type, var_or_fn_ref(name, type), ctx}

      :error ->
        {:error, "Undefined variable: #{name}"}
    end
  end

  # Bare atom - could be variable or literal atom
  defp infer_expr(a, ctx) when is_atom(a) do
    case Context.lookup(ctx, a) do
      {:ok, scheme} ->
        {type, ctx} = Context.instantiate(ctx, scheme)
        {:ok, type, var_or_fn_ref(a, type), ctx}

      :error ->
        # It's a literal atom
        {:ok, {:atom, a}, {:lit, :atom, a}, ctx}
    end
  end

  # Function reference node
  defp infer_expr({:fn_ref, name, arity}, ctx) when is_atom(name) do
    case Context.lookup(ctx, name) do
      {:ok, scheme} ->
        {type, ctx} = Context.instantiate(ctx, scheme)
        {:ok, type, {:fn_ref, name, arity, type}, ctx}

      :error ->
        {:error, "Undefined function reference: #{name}/#{arity}"}
    end
  end

  # --- Anonymous Function ---
  # (fn [x y] body) → infer types for params, then body

  defp infer_expr({:fn, params, body}, ctx) do
    # Generate fresh type variables for each parameter
    {param_tvars, ctx} = Context.fresh_vars(ctx, length(params))

    # Extend environment with params bound to their tvars
    param_bindings = Enum.zip(params, param_tvars)
    ctx = Context.extend_many(ctx, param_bindings)

    # Infer body type
    case infer_expr(body, ctx) do
      {:ok, body_type, typed_body, ctx} ->
        # Function type is params → body
        func_type = {:fn, param_tvars, body_type}
        {:ok, func_type, {:fn, params, typed_body, func_type}, ctx}

      error ->
        error
    end
  end

  # Strip location from fn nodes
  defp infer_expr({:fn, params, body, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:fn, params, body}, ctx)
  end

  # --- List Builtins ---

  defp infer_expr({:call, :head, [list_expr]}, ctx) do
    case infer_expr(list_expr, ctx) do
      {:ok, {:list, elem_type}, typed_list, ctx} ->
        {:ok, elem_type, {:call, :head, [typed_list], elem_type}, ctx}

      {:ok, :any, typed_list, ctx} ->
        {:ok, :any, {:call, :head, [typed_list], :any}, ctx}

      {:ok, other_type, _typed_list, _ctx} ->
        {:error, "head expects a list, got #{Core.format_type(other_type)}"}

      error ->
        error
    end
  end

  defp infer_expr({:call, :tail, [list_expr]}, ctx) do
    case infer_expr(list_expr, ctx) do
      {:ok, {:list, _} = list_type, typed_list, ctx} ->
        {:ok, list_type, {:call, :tail, [typed_list], list_type}, ctx}

      {:ok, :any, typed_list, ctx} ->
        list_type = {:list, :any}
        {:ok, list_type, {:call, :tail, [typed_list], list_type}, ctx}

      {:ok, other_type, _typed_list, _ctx} ->
        {:error, "tail expects a list, got #{Core.format_type(other_type)}"}

      error ->
        error
    end
  end

  defp infer_expr({:call, :empty?, [list_expr]}, ctx) do
    case infer_expr(list_expr, ctx) do
      {:ok, {:list, _}, typed_list, ctx} ->
        {:ok, :bool, {:call, :empty?, [typed_list], :bool}, ctx}

      {:ok, :any, typed_list, ctx} ->
        {:ok, :bool, {:call, :empty?, [typed_list], :bool}, ctx}

      {:ok, other_type, _typed_list, _ctx} ->
        {:error, "empty? expects a list, got #{Core.format_type(other_type)}"}

      error ->
        error
    end
  end

  defp infer_expr({:call, :length, [list_expr]}, ctx) do
    case infer_expr(list_expr, ctx) do
      {:ok, {:list, _}, typed_list, ctx} ->
        {:ok, :int, {:call, :length, [typed_list], :int}, ctx}

      {:ok, :any, typed_list, ctx} ->
        {:ok, :int, {:call, :length, [typed_list], :int}, ctx}

      {:ok, other_type, _typed_list, _ctx} ->
        {:error, "length expects a list, got #{Core.format_type(other_type)}"}

      error ->
        error
    end
  end

  # --- Str (variadic string concat) ---

  defp infer_expr({:call, :str, args}, ctx) when length(args) > 0 do
    case infer_all_elements(args, ctx, []) do
      {:ok, typed_args, ctx} ->
        {:ok, :string, {:call, :str, typed_args, :string}, ctx}

      error ->
        error
    end
  end

  # --- Map Builtin ---

  defp infer_expr({:call, :map, [func_expr, list_expr]}, ctx) do
    with {:ok, func_type, typed_func, ctx} <- infer_expr(func_expr, ctx),
         {:ok, _list_type, typed_list, ctx} <- infer_expr(list_expr, ctx) do
      case func_type do
        {:fn, [_arg_type], ret_type} ->
          result_type = {:list, ret_type}
          {:ok, result_type, {:call, :map, [typed_func, typed_list], result_type}, ctx}

        {:fn, args, _} ->
          {:error, "map expects a function with 1 argument, got #{length(args)}"}

        :any ->
          {:ok, {:list, :any}, {:call, :map, [typed_func, typed_list], {:list, :any}}, ctx}

        _ ->
          {:error, "map expects a function as first argument, got #{Core.format_type(func_type)}"}
      end
    end
  end

  # --- Filter Builtin ---

  defp infer_expr({:call, :filter, [func_expr, list_expr]}, ctx) do
    with {:ok, func_type, typed_func, ctx} <- infer_expr(func_expr, ctx),
         {:ok, list_type, typed_list, ctx} <- infer_expr(list_expr, ctx) do
      case func_type do
        {:fn, [_arg_type], ret_type} when ret_type in [:bool, :any] ->
          result_type = list_type
          {:ok, result_type, {:call, :filter, [typed_func, typed_list], result_type}, ctx}

        {:fn, [_arg_type], ret_type} ->
          {:error, "filter predicate must return Bool, got #{Core.format_type(ret_type)}"}

        {:fn, args, _} ->
          {:error, "filter expects a function with 1 argument, got #{length(args)}"}

        :any ->
          {:ok, list_type, {:call, :filter, [typed_func, typed_list], list_type}, ctx}

        _ ->
          {:error, "filter expects a function as first argument, got #{Core.format_type(func_type)}"}
      end
    end
  end

  # --- Fold Builtin ---

  defp infer_expr({:call, :fold, [func_expr, init_expr, list_expr]}, ctx) do
    with {:ok, func_type, typed_func, ctx} <- infer_expr(func_expr, ctx),
         {:ok, init_type, typed_init, ctx} <- infer_expr(init_expr, ctx),
         {:ok, _list_type, typed_list, ctx} <- infer_expr(list_expr, ctx) do
      case func_type do
        {:fn, [_acc_type, _elem_type], ret_type} ->
          {:ok, ret_type, {:call, :fold, [typed_func, typed_init, typed_list], ret_type}, ctx}

        {:fn, args, _} ->
          {:error, "fold expects a function with 2 arguments, got #{length(args)}"}

        :any ->
          {:ok, init_type, {:call, :fold, [typed_func, typed_init, typed_list], init_type}, ctx}

        _ ->
          {:error, "fold expects a function as first argument, got #{Core.format_type(func_type)}"}
      end
    end
  end

  # --- Qualified Calls ---
  # (Mod/func args...) → lookup extern type or fall back to :any

  defp infer_expr({:call, {:qualified, mod, func}, args}, ctx) do
    extern_name = :"#{mod}:#{func}"

    case Context.lookup(ctx, extern_name) do
      {:ok, {:fn, _param_types, ret_type}} ->
        case infer_all_elements(args, ctx, []) do
          {:ok, typed_args, ctx} ->
            {:ok, ret_type, {:call, {:qualified, mod, func}, typed_args, ret_type}, ctx}

          error ->
            error
        end

      {:ok, _other} ->
        {:error, "#{mod}:#{func} is not a function"}

      :error ->
        # Not registered — allow with :any (dynamic interop)
        case infer_all_elements(args, ctx, []) do
          {:ok, typed_args, ctx} ->
            {:ok, :any, {:call, {:qualified, mod, func}, typed_args, :any}, ctx}

          error ->
            error
        end
    end
  end

  defp infer_expr({:call, {:qualified, mod, func}, args, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:call, {:qualified, mod, func}, args}, ctx)
  end

  # --- Function Application ---

  defp infer_expr({:call, func, args}, ctx) when is_atom(func) do
    case Context.lookup(ctx, func) do
      {:ok, raw_type} ->
        # Wrap bare constructor types as schemes for fresh instantiation
        scheme = ensure_scheme(raw_type)
        {func_type, ctx} = Context.instantiate(ctx, scheme)
        infer_application(func, func_type, args, ctx)

      :error ->
        {:error, "Unknown function: #{func}"}
    end
  end

  defp infer_expr({:call, func, args, %Vaisto.Parser.Loc{} = loc}, ctx) do
    case infer_expr({:call, func, args}, ctx) do
      {:ok, type, ast, ctx} -> {:ok, type, ast, ctx}
      {:error, msg} -> {:error, with_location(msg, loc)}
    end
  end

  # --- Higher-order Apply ---
  # (expr args...) where expr is not an atom — call a function expression

  defp infer_expr({:call, func_expr, args}, ctx) do
    with {:ok, func_type, typed_func, ctx} <- infer_expr(func_expr, ctx) do
      {ret_tvar, ctx} = Context.fresh_var(ctx)
      {arg_tvars, ctx} = Context.fresh_vars(ctx, length(args))
      expected_fn_type = {:fn, arg_tvars, ret_tvar}

      case Context.unify_types(ctx, func_type, expected_fn_type) do
        {:ok, ctx} ->
          case infer_and_unify_args(args, arg_tvars, ctx, []) do
            {:ok, typed_args, ctx} ->
              {:ok, ret_tvar, {:apply, typed_func, typed_args, ret_tvar}, ctx}

            error ->
              error
          end

        {:error, reason} ->
          {:error, "Cannot call non-function: #{reason}"}
      end
    end
  end


  # --- Let Bindings ---
  # (let [x expr1] body) - with let-polymorphism

  defp infer_expr({:let, bindings, body}, ctx) do
    case infer_let_bindings(bindings, ctx, []) do
      {:ok, ctx, typed_bindings} ->
        case infer_expr(body, ctx) do
          {:ok, body_type, typed_body, ctx} ->
            {:ok, body_type, {:let, typed_bindings, typed_body, body_type}, ctx}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp infer_expr({:let, bindings, body, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:let, bindings, body}, ctx)
  end

  # --- If Expression ---

  defp infer_expr({:if, cond_expr, then_expr, else_expr}, ctx) do
    with {:ok, cond_type, typed_cond, ctx} <- infer_expr(cond_expr, ctx),
         {:ok, ctx} <- Context.unify_types(ctx, cond_type, :bool),
         {:ok, then_type, typed_then, ctx} <- infer_expr(then_expr, ctx),
         {:ok, else_type, typed_else, ctx} <- infer_expr(else_expr, ctx),
         {:ok, ctx} <- Context.unify_types(ctx, then_type, else_type) do
      {:ok, then_type, {:if, typed_cond, typed_then, typed_else, then_type}, ctx}
    end
  end

  defp infer_expr({:if, cond_expr, then_expr, else_expr, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:if, cond_expr, then_expr, else_expr}, ctx)
  end

  # --- List Literal ---

  defp infer_expr({:list, []}, ctx) do
    # Empty list has polymorphic element type
    {elem_tvar, ctx} = Context.fresh_var(ctx)
    list_type = {:list, elem_tvar}
    {:ok, list_type, {:list, [], list_type}, ctx}
  end

  defp infer_expr({:list, elements}, ctx) do
    # Generate fresh var for element type
    {elem_tvar, ctx} = Context.fresh_var(ctx)

    # Infer each element and unify with elem_tvar
    case infer_list_elements(elements, elem_tvar, ctx, []) do
      {:ok, typed_elements, ctx} ->
        list_type = {:list, elem_tvar}
        {:ok, list_type, {:list, typed_elements, list_type}, ctx}

      error ->
        error
    end
  end

  defp infer_expr({:list, elements, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:list, elements}, ctx)
  end

  # --- Unit ---

  defp infer_expr({:unit}, ctx) do
    {:ok, :unit, {:unit}, ctx}
  end

  defp infer_expr({:unit, %Vaisto.Parser.Loc{}}, ctx) do
    {:ok, :unit, {:unit}, ctx}
  end

  # --- Field Access ---
  # (. record :field) → row-polymorphic field type

  defp infer_expr({:field_access, record_expr, field}, ctx) when is_atom(field) do
    case infer_expr(record_expr, ctx) do
      {:ok, record_type, typed_record, ctx} ->
        # Fresh type variable for the field's type
        {field_tvar, ctx} = Context.fresh_var(ctx)
        # Fresh row variable for the open tail
        {row_rvar, ctx} = Context.fresh_row_var(ctx)

        # Unify the record type with a row requiring at least {field: field_tvar}
        row_constraint = {:row, [{field, field_tvar}], row_rvar}

        case Context.unify_types(ctx, record_type, row_constraint) do
          {:ok, ctx} ->
            {:ok, field_tvar, {:field_access, typed_record, field, field_tvar}, ctx}

          {:error, reason} ->
            {:error, "Field access error: #{reason}"}
        end

      error ->
        error
    end
  end

  # Strip location from field_access nodes
  defp infer_expr({:field_access, record_expr, field, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:field_access, record_expr, field}, ctx)
  end

  # --- Do Blocks ---
  # (do expr1 expr2 ... exprN) → type of last expression, :unit if empty

  defp infer_expr({:do, []}, ctx) do
    {:ok, :unit, {:do, [], :unit}, ctx}
  end

  defp infer_expr({:do, exprs}, ctx) do
    case infer_exprs_sequence(exprs, ctx, []) do
      {:ok, typed_exprs, last_type, ctx} ->
        {:ok, last_type, {:do, typed_exprs, last_type}, ctx}

      error ->
        error
    end
  end

  defp infer_expr({:do, exprs, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:do, exprs}, ctx)
  end

  # --- Tuple Expressions ---
  # Tuples are for Erlang interop, typed as :any (matches TypeChecker)

  defp infer_expr({:tuple, elements}, ctx) when is_list(elements) do
    case infer_all_elements(elements, ctx, []) do
      {:ok, typed_elements, ctx} ->
        {:ok, :any, {:tuple, typed_elements, :any}, ctx}

      error ->
        error
    end
  end

  defp infer_expr({:tuple, elements, %Vaisto.Parser.Loc{}}, ctx) when is_list(elements) do
    infer_expr({:tuple, elements}, ctx)
  end

  defp infer_expr({:tuple_pattern, elements}, ctx) when is_list(elements) do
    case infer_all_elements(elements, ctx, []) do
      {:ok, typed_elements, ctx} ->
        {:ok, :any, {:tuple, typed_elements, :any}, ctx}

      error ->
        error
    end
  end

  # --- Cons Expressions ---
  # (cons head tail) → {:list, elem_type}

  defp infer_expr({:cons, head, tail}, ctx) do
    {elem_tvar, ctx} = Context.fresh_var(ctx)

    with {:ok, head_type, typed_head, ctx} <- infer_expr(head, ctx),
         {:ok, ctx} <- Context.unify_types(ctx, head_type, elem_tvar),
         {:ok, tail_type, typed_tail, ctx} <- infer_expr(tail, ctx),
         {:ok, ctx} <- Context.unify_types(ctx, tail_type, {:list, elem_tvar}) do
      list_type = {:list, elem_tvar}
      {:ok, list_type, {:cons, typed_head, typed_tail, list_type}, ctx}
    end
  end

  defp infer_expr({:cons, head, tail, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:cons, head, tail}, ctx)
  end

  # --- Bracket Expressions ---
  # Brackets normalize into list or cons typed ASTs

  defp infer_expr({:bracket, []}, ctx) do
    infer_expr({:list, []}, ctx)
  end

  defp infer_expr({:bracket, {:cons, head, tail}}, ctx) do
    infer_expr({:cons, head, tail}, ctx)
  end

  defp infer_expr({:bracket, elements}, ctx) when is_list(elements) do
    infer_expr({:list, elements}, ctx)
  end

  defp infer_expr({:bracket, content, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:bracket, content}, ctx)
  end

  # --- Match Expressions ---
  # (match scrutinee [pattern1 body1] [pattern2 body2] ...)

  defp infer_expr({:match, scrutinee, clauses}, ctx) do
    with {:ok, scrutinee_type, typed_scrutinee, ctx} <- infer_expr(scrutinee, ctx) do
      {result_tvar, ctx} = Context.fresh_var(ctx)

      case infer_match_clauses(clauses, scrutinee_type, result_tvar, ctx, []) do
        {:ok, typed_clauses, ctx} ->
          {:ok, result_tvar, {:match, typed_scrutinee, typed_clauses, result_tvar}, ctx}

        error ->
          error
      end
    end
  end

  defp infer_expr({:match, scrutinee, clauses, %Vaisto.Parser.Loc{}}, ctx) do
    infer_expr({:match, scrutinee, clauses}, ctx)
  end

  # --- Catch-all for unhandled expressions ---
  # Fall back to the original type checker for complex forms

  defp infer_expr(other, _ctx) do
    {:error, "Inference not implemented for: #{inspect(other)}"}
  end

  # --- Helpers ---

  # Emit {:fn_ref, ...} for function types used as values, {:var, ...} otherwise
  defp var_or_fn_ref(name, {:fn, params, _} = type) do
    {:fn_ref, name, length(params), type}
  end

  defp var_or_fn_ref(name, type), do: {:var, name, type}

  defp infer_application(func_name, func_type, args, ctx) do
    case func_type do
      {:fn, param_types, ret_type} ->
        if length(param_types) != length(args) do
          {:error, "Arity mismatch: #{func_name} expects #{length(param_types)} args, got #{length(args)}"}
        else
          # Infer each argument and unify with expected param type
          case infer_and_unify_args(args, param_types, ctx, []) do
            {:ok, typed_args, ctx} ->
              {:ok, ret_type, {:call, func_name, typed_args, ret_type}, ctx}

            error ->
              error
          end
        end

      other ->
        {:error, "Cannot call non-function: #{inspect(other)}"}
    end
  end

  defp infer_and_unify_args([], [], ctx, acc) do
    {:ok, Enum.reverse(acc), ctx}
  end

  defp infer_and_unify_args([arg | rest_args], [param_type | rest_params], ctx, acc) do
    case infer_expr(arg, ctx) do
      {:ok, arg_type, typed_arg, ctx} ->
        case Context.unify_types(ctx, arg_type, param_type) do
          {:ok, ctx} ->
            infer_and_unify_args(rest_args, rest_params, ctx, [typed_arg | acc])

          {:error, reason} ->
            {:error, "Type mismatch in argument: #{reason}"}
        end

      error ->
        error
    end
  end

  defp infer_let_bindings([], ctx, acc) do
    {:ok, ctx, Enum.reverse(acc)}
  end

  defp infer_let_bindings([{name, expr} | rest], ctx, acc) do
    case infer_expr(expr, ctx) do
      {:ok, type, typed_expr, ctx} ->
        # Generalize the type for let-polymorphism
        scheme = Context.generalize(ctx, type)
        ctx = Context.extend(ctx, name, scheme)
        infer_let_bindings(rest, ctx, [{name, typed_expr, type} | acc])

      error ->
        error
    end
  end

  defp infer_list_elements([], _elem_tvar, ctx, acc) do
    {:ok, Enum.reverse(acc), ctx}
  end

  defp infer_list_elements([elem | rest], elem_tvar, ctx, acc) do
    case infer_expr(elem, ctx) do
      {:ok, elem_type, typed_elem, ctx} ->
        case Context.unify_types(ctx, elem_type, elem_tvar) do
          {:ok, ctx} ->
            infer_list_elements(rest, elem_tvar, ctx, [typed_elem | acc])

          {:error, reason} ->
            {:error, "List element type mismatch: #{reason}"}
        end

      error ->
        error
    end
  end

  # --- Do block helper ---

  defp infer_exprs_sequence([expr], ctx, acc) do
    case infer_expr(expr, ctx) do
      {:ok, type, typed_expr, ctx} ->
        {:ok, Enum.reverse([typed_expr | acc]), type, ctx}

      error ->
        error
    end
  end

  defp infer_exprs_sequence([expr | rest], ctx, acc) do
    case infer_expr(expr, ctx) do
      {:ok, _type, typed_expr, ctx} ->
        infer_exprs_sequence(rest, ctx, [typed_expr | acc])

      error ->
        error
    end
  end

  # --- Generic element inference helper ---

  defp infer_all_elements([], ctx, acc) do
    {:ok, Enum.reverse(acc), ctx}
  end

  defp infer_all_elements([elem | rest], ctx, acc) do
    case infer_expr(elem, ctx) do
      {:ok, _type, typed_elem, ctx} ->
        infer_all_elements(rest, ctx, [typed_elem | acc])

      error ->
        error
    end
  end

  # --- Match helpers ---

  defp infer_match_clauses([], _scrutinee_type, _result_tvar, ctx, acc) do
    {:ok, Enum.reverse(acc), ctx}
  end

  defp infer_match_clauses([{pattern, body} | rest], scrutinee_type, result_tvar, ctx, acc) do
    saved_env = ctx.env

    case infer_pattern(pattern, scrutinee_type, ctx) do
      {:ok, bindings, typed_pattern, ctx} ->
        ctx = Context.extend_many(ctx, bindings)

        case infer_expr(body, ctx) do
          {:ok, body_type, typed_body, ctx} ->
            case Context.unify_types(ctx, body_type, result_tvar) do
              {:ok, ctx} ->
                typed_clause = {typed_pattern, typed_body, body_type}
                ctx = %{ctx | env: saved_env}
                infer_match_clauses(rest, scrutinee_type, result_tvar, ctx, [typed_clause | acc])

              {:error, _} = err ->
                err
            end

          error ->
            error
        end

      {:error, _} = err ->
        err
    end
  end

  # --- Scheme helpers ---
  # Wraps bare constructor {:fn, ...} types as {:forall, ...} so
  # Context.instantiate generates fresh tvars at each use site
  defp ensure_scheme({:forall, _, _} = s), do: s
  defp ensure_scheme({:fn, _, _} = fn_type) do
    tvars = Core.free_vars(fn_type) |> MapSet.to_list() |> Enum.filter(&is_integer/1)
    if tvars == [], do: fn_type, else: {:forall, tvars, fn_type}
  end
  defp ensure_scheme(type), do: type

  # --- Pattern inference ---
  # Returns {:ok, bindings, typed_pattern, ctx}

  # Wildcard
  defp infer_pattern(:_, _scrutinee_type, ctx) do
    {:ok, [], :_, ctx}
  end

  # Boolean literals — must come before the is_atom guard since true/false are atoms
  defp infer_pattern(true, scrutinee_type, ctx) do
    case Context.unify_types(ctx, scrutinee_type, :bool) do
      {:ok, ctx} -> {:ok, [], {:lit, :bool, true}, ctx}
      {:error, reason} -> {:error, reason}
    end
  end

  defp infer_pattern(false, scrutinee_type, ctx) do
    case Context.unify_types(ctx, scrutinee_type, :bool) do
      {:ok, ctx} -> {:ok, [], {:lit, :bool, false}, ctx}
      {:error, reason} -> {:error, reason}
    end
  end

  # Variable
  defp infer_pattern(name, scrutinee_type, ctx) when is_atom(name) do
    {:ok, [{name, scrutinee_type}], {:var, name, scrutinee_type}, ctx}
  end

  defp infer_pattern({:var, name}, scrutinee_type, ctx) when is_atom(name) do
    {:ok, [{name, scrutinee_type}], {:var, name, scrutinee_type}, ctx}
  end

  # Integer literal
  defp infer_pattern(n, scrutinee_type, ctx) when is_integer(n) do
    case Context.unify_types(ctx, scrutinee_type, :int) do
      {:ok, ctx} -> {:ok, [], {:lit, :int, n}, ctx}
      {:error, reason} -> {:error, reason}
    end
  end

  # Float literal
  defp infer_pattern(f, scrutinee_type, ctx) when is_float(f) do
    case Context.unify_types(ctx, scrutinee_type, :float) do
      {:ok, ctx} -> {:ok, [], {:lit, :float, f}, ctx}
      {:error, reason} -> {:error, reason}
    end
  end

  # Atom literal
  defp infer_pattern({:atom, a}, _scrutinee_type, ctx) do
    {:ok, [], {:lit, :atom, a}, ctx}
  end

  # String literal
  defp infer_pattern({:string, s}, scrutinee_type, ctx) do
    case Context.unify_types(ctx, scrutinee_type, :string) do
      {:ok, ctx} -> {:ok, [], {:lit, :string, s}, ctx}
      {:error, reason} -> {:error, reason}
    end
  end

  # Empty list pattern
  defp infer_pattern([], scrutinee_type, ctx) do
    {elem_tvar, ctx} = Context.fresh_var(ctx)
    case Context.unify_types(ctx, scrutinee_type, {:list, elem_tvar}) do
      {:ok, ctx} ->
        {:ok, [], {:list_pattern, [], {:list, elem_tvar}}, ctx}
      {:error, reason} ->
        {:error, reason}
    end
  end

  # Cons pattern [h | t]
  defp infer_pattern({:cons, head, tail}, scrutinee_type, ctx) do
    {elem_tvar, ctx} = Context.fresh_var(ctx)
    list_type = {:list, elem_tvar}

    ctx = case Context.unify_types(ctx, scrutinee_type, list_type) do
      {:ok, ctx} -> ctx
      {:error, _} -> ctx
    end

    {:ok, head_bindings, typed_head, ctx} = infer_pattern(head, elem_tvar, ctx)
    {:ok, tail_bindings, typed_tail, ctx} = infer_pattern(tail, list_type, ctx)

    {:ok, head_bindings ++ tail_bindings, {:cons_pattern, typed_head, typed_tail, list_type}, ctx}
  end

  defp infer_pattern({:cons, head, tail, %Vaisto.Parser.Loc{}}, scrutinee_type, ctx) do
    infer_pattern({:cons, head, tail}, scrutinee_type, ctx)
  end

  # Bracket pattern
  defp infer_pattern({:bracket, []}, scrutinee_type, ctx) do
    infer_pattern([], scrutinee_type, ctx)
  end

  defp infer_pattern({:bracket, {:cons, head, tail}}, scrutinee_type, ctx) do
    infer_pattern({:cons, head, tail}, scrutinee_type, ctx)
  end

  defp infer_pattern({:bracket, elements}, scrutinee_type, ctx) when is_list(elements) do
    infer_pattern(elements, scrutinee_type, ctx)
  end

  # Tuple pattern
  defp infer_pattern({:tuple_pattern, elements}, _scrutinee_type, ctx) do
    {:ok, bindings, typed_elems, ctx} = infer_pattern_elements(elements, :any, ctx, [], [])
    {:ok, bindings, {:tuple_pattern, typed_elems, :any}, ctx}
  end

  defp infer_pattern({:tuple, elements, %Vaisto.Parser.Loc{}}, scrutinee_type, ctx) when is_list(elements) do
    infer_pattern({:tuple_pattern, elements}, scrutinee_type, ctx)
  end

  # Constructor pattern: (CtorName arg1 arg2 ...)
  defp infer_pattern({:call, name, args, %Vaisto.Parser.Loc{}}, scrutinee_type, ctx) do
    infer_pattern({:call, name, args}, scrutinee_type, ctx)
  end

  defp infer_pattern({:call, name, args}, scrutinee_type, ctx) when is_atom(name) do
    case Context.lookup(ctx, name) do
      {:ok, raw_type} ->
        scheme = ensure_scheme(raw_type)
        {ctor_type, ctx} = Context.instantiate(ctx, scheme)

        case ctor_type do
          {:fn, param_types, ret_type} ->
            # Unify return type with scrutinee to propagate constraints
            ctx = case Context.unify_types(ctx, ret_type, scrutinee_type) do
              {:ok, ctx} -> ctx
              {:error, _} -> ctx
            end
            {:ok, bindings, typed_args, ctx} = infer_pattern_elements(args, param_types, ctx, [], [])
            {:ok, bindings, {:pattern, name, typed_args, scrutinee_type}, ctx}

          _ ->
            {:ok, bindings, typed_args, ctx} = infer_pattern_elements(
              args, List.duplicate(:any, length(args)), ctx, [], [])
            {:ok, bindings, {:pattern, name, typed_args, scrutinee_type}, ctx}
        end

      :error ->
        {:ok, bindings, typed_args, ctx} = infer_pattern_elements(
          args, List.duplicate(:any, length(args)), ctx, [], [])
        {:ok, bindings, {:pattern, name, typed_args, scrutinee_type}, ctx}
    end
  end

  # Catch-all pattern — treat as opaque
  defp infer_pattern(_pattern, _scrutinee_type, ctx) do
    {:ok, [], :_, ctx}
  end

  defp infer_pattern_elements([], _types, ctx, bindings_acc, elems_acc) do
    {:ok, Enum.reverse(bindings_acc), Enum.reverse(elems_acc), ctx}
  end

  defp infer_pattern_elements([elem | rest], types, ctx, bindings_acc, elems_acc) do
    {elem_type, rest_types} = case types do
      [t | ts] -> {t, ts}
      t when is_atom(t) -> {t, t}
      _ -> {:any, :any}
    end

    {:ok, bindings, typed_elem, ctx} = infer_pattern(elem, elem_type, ctx)
    infer_pattern_elements(rest, rest_types, ctx, Enum.reverse(bindings) ++ bindings_acc, [typed_elem | elems_acc])
  end

  defp with_location(msg, %Vaisto.Parser.Loc{line: line, col: col, file: file}) do
    prefix = case file do
      nil -> "#{line}:#{col}"
      f -> "#{f}:#{line}:#{col}"
    end
    "#{prefix}: #{msg}"
  end

  # Apply substitution to typed AST
  defp apply_subst_to_ast(ast, subst) when map_size(subst) == 0, do: ast

  defp apply_subst_to_ast({:lit, type, val}, _subst), do: {:lit, type, val}

  defp apply_subst_to_ast({:var, name, type}, subst) do
    {:var, name, Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:apply, func, args, type}, subst) do
    {:apply, apply_subst_to_ast(func, subst), Enum.map(args, &apply_subst_to_ast(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:fn_ref, name, arity, type}, subst) do
    {:fn_ref, name, arity, Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:fn, params, body, type}, subst) do
    {:fn, params, apply_subst_to_ast(body, subst), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:call, func, args, type}, subst) do
    {:call, func, Enum.map(args, &apply_subst_to_ast(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:let, bindings, body, type}, subst) do
    typed_bindings = Enum.map(bindings, fn {name, expr, t} ->
      {name, apply_subst_to_ast(expr, subst), Core.apply_subst(subst, t)}
    end)
    {:let, typed_bindings, apply_subst_to_ast(body, subst), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:if, cond, then_b, else_b, type}, subst) do
    {:if,
     apply_subst_to_ast(cond, subst),
     apply_subst_to_ast(then_b, subst),
     apply_subst_to_ast(else_b, subst),
     Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:list, elems, type}, subst) do
    {:list, Enum.map(elems, &apply_subst_to_ast(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:field_access, record, field, type}, subst) do
    {:field_access, apply_subst_to_ast(record, subst), field, Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:unit}, _subst), do: {:unit}

  defp apply_subst_to_ast({:do, exprs, type}, subst) do
    {:do, Enum.map(exprs, &apply_subst_to_ast(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:tuple, elems, type}, subst) do
    {:tuple, Enum.map(elems, &apply_subst_to_ast(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:cons, head, tail, type}, subst) do
    {:cons, apply_subst_to_ast(head, subst), apply_subst_to_ast(tail, subst), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast({:match, scrutinee, clauses, type}, subst) do
    typed_clauses = Enum.map(clauses, fn {pattern, body, body_type} ->
      {apply_subst_to_pattern(pattern, subst), apply_subst_to_ast(body, subst), Core.apply_subst(subst, body_type)}
    end)
    {:match, apply_subst_to_ast(scrutinee, subst), typed_clauses, Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_ast(other, _subst), do: other

  # --- Pattern substitution ---

  defp apply_subst_to_pattern({:var, name, type}, subst) do
    {:var, name, Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_pattern({:cons_pattern, head, tail, type}, subst) do
    {:cons_pattern, apply_subst_to_pattern(head, subst), apply_subst_to_pattern(tail, subst), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_pattern({:list_pattern, elems, type}, subst) do
    {:list_pattern, Enum.map(elems, &apply_subst_to_pattern(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_pattern({:tuple_pattern, elems, type}, subst) do
    {:tuple_pattern, Enum.map(elems, &apply_subst_to_pattern(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_pattern({:pattern, name, args, type}, subst) do
    {:pattern, name, Enum.map(args, &apply_subst_to_pattern(&1, subst)), Core.apply_subst(subst, type)}
  end

  defp apply_subst_to_pattern(other, _subst), do: other
end
