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
        # Instantiate if polymorphic
        {type, ctx} = Context.instantiate(ctx, scheme)
        {:ok, type, {:var, name, type}, ctx}

      :error ->
        {:error, "Undefined variable: #{name}"}
    end
  end

  # Bare atom - could be variable or literal atom
  defp infer_expr(a, ctx) when is_atom(a) do
    case Context.lookup(ctx, a) do
      {:ok, scheme} ->
        {type, ctx} = Context.instantiate(ctx, scheme)
        {:ok, type, {:var, a, type}, ctx}

      :error ->
        # It's a literal atom
        {:ok, {:atom, a}, {:lit, :atom, a}, ctx}
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

  # --- Function Application ---

  defp infer_expr({:call, func, args}, ctx) when is_atom(func) do
    case Context.lookup(ctx, func) do
      {:ok, scheme} ->
        # Instantiate function type (handles polymorphism)
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

  # --- Catch-all for unhandled expressions ---
  # Fall back to the original type checker for complex forms

  defp infer_expr(other, _ctx) do
    {:error, "Inference not implemented for: #{inspect(other)}"}
  end

  # --- Helpers ---

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

  defp apply_subst_to_ast(other, _subst), do: other
end
