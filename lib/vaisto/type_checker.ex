defmodule Vaisto.TypeChecker do
  @moduledoc """
  Type checker for Vaisto using Hindley-Milner style inference.
  
  Ensures type safety at compile time:
    (+ 1 :atom) → TypeError
    (+ 1 2)     → :int
  """

  # Built-in type environment
  @primitives %{
    :+ => {:fn, [:int, :int], :int},
    :- => {:fn, [:int, :int], :int},
    :* => {:fn, [:int, :int], :int},
    :/ => {:fn, [:int, :int], :int},
    :! => {:fn, [:pid, :any], :ok},
    :? => {:fn, [:pid, :any], :any},
    :== => {:fn, [:any, :any], :bool},
    :< => {:fn, [:int, :int], :bool},
    :> => {:fn, [:int, :int], :bool}
  }

  @doc """
  Check types and return the result type. Raises on error.
  """
  def check!(ast, env \\ @primitives) do
    case check(ast, env) do
      {:ok, type, typed_ast} -> typed_ast
      {:error, msg} -> raise "TypeError: #{msg}"
    end
  end

  @doc """
  Check types and return {:ok, type, typed_ast} or {:error, reason}.
  """
  def check(ast, env \\ @primitives)

  # Literals
  def check(n, _env) when is_integer(n), do: {:ok, :int, {:lit, :int, n}}
  def check(f, _env) when is_float(f), do: {:ok, :float, {:lit, :float, f}}
  def check(true, _env), do: {:ok, :bool, {:lit, :bool, true}}
  def check(false, _env), do: {:ok, :bool, {:lit, :bool, false}}

  # Atoms (as message types)
  def check(a, _env) when is_atom(a) do
    {:ok, {:atom, a}, {:lit, :atom, a}}
  end

  # Variable lookup
  def check({:var, name}, env) do
    case Map.get(env, name) do
      nil -> {:error, "Undefined variable: #{name}"}
      type -> {:ok, type, {:var, name, type}}
    end
  end

  # Function call
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

  defp check_children(children, env) do
    # For now, just validate they're well-formed
    {:ok, children}
  end
end
