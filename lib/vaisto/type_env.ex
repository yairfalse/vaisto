defmodule Vaisto.TypeEnv do
  @moduledoc """
  Type environment for Vaisto's type checker.

  A type environment maps names to their types:
  - Function names → function types `{:fn, [arg_types], ret_type}`
  - Variable names → their inferred/declared types
  - Process names → process types `{:process, state_type, msg_types}`
  - Type names → type definitions

  ## Structure

  The environment is a map with the following keys:
  - Atoms for function/variable names → types
  - `:__local_vars__` → set of local variable names (for distinguishing from module functions)

  ## Example

      env = TypeEnv.new()
      env = TypeEnv.put(env, :add, {:fn, [:int, :int], :int})
      {:fn, [:int, :int], :int} = TypeEnv.get(env, :add)
  """

  # Built-in primitives available in every environment
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
    :!= => {:fn, [:any, :any], :bool},
    # Type class method signatures (constrained polymorphic)
    :eq => {:forall, [0], {:constrained, [{:Eq, {:tvar, 0}}], {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}}},
    :neq => {:forall, [0], {:constrained, [{:Eq, {:tvar, 0}}], {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}}},
    :show => {:forall, [0], {:constrained, [{:Show, {:tvar, 0}}], {:fn, [{:tvar, 0}], :string}}},
    # Type class registry
    # Format: {:class, name, tvar_ids, method_sigs, defaults}
    :__classes__ => %{
      Eq: {:class, :Eq, [0], [
        {:eq, {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}},
        {:neq, {:fn, [{:tvar, 0}, {:tvar, 0}], :bool}}
      ], %{
        neq: {:default, [:x, :y],
          {:if, {:call, :eq, [:x, :y], nil}, false, true, nil}}
      }},
      Show: {:class, :Show, [0], [
        {:show, {:fn, [{:tvar, 0}], :string}}
      ], %{}}
    },
    # Instance registry: {ClassName, ConcreteType} => %{method => type}
    :__instances__ => %{
      {:Eq, :int} => %{eq: {:fn, [:int, :int], :bool}, neq: {:fn, [:int, :int], :bool}},
      {:Eq, :float} => %{eq: {:fn, [:float, :float], :bool}, neq: {:fn, [:float, :float], :bool}},
      {:Eq, :string} => %{eq: {:fn, [:string, :string], :bool}, neq: {:fn, [:string, :string], :bool}},
      {:Eq, :bool} => %{eq: {:fn, [:bool, :bool], :bool}, neq: {:fn, [:bool, :bool], :bool}},
      {:Eq, :atom} => %{eq: {:fn, [:atom, :atom], :bool}, neq: {:fn, [:atom, :atom], :bool}},
      {:Show, :int} => %{show: {:fn, [:int], :string}},
      {:Show, :float} => %{show: {:fn, [:float], :string}},
      {:Show, :string} => %{show: {:fn, [:string], :string}},
      {:Show, :bool} => %{show: {:fn, [:bool], :string}}
    }
  }

  @doc """
  Creates a new type environment with built-in primitives.
  """
  @spec new() :: map()
  def new do
    @primitives
  end

  @doc """
  Returns the built-in primitives.
  """
  @spec primitives() :: map()
  def primitives, do: @primitives

  @doc """
  Gets a type from the environment.
  Returns `nil` if not found.
  """
  @spec get(map(), atom()) :: term() | nil
  def get(env, name) when is_atom(name) do
    Map.get(env, name)
  end

  @doc """
  Puts a type into the environment.
  """
  @spec put(map(), atom(), term()) :: map()
  def put(env, name, type) when is_atom(name) do
    Map.put(env, name, type)
  end

  @doc """
  Merges another environment or map into this one.
  Values in `other` override values in `env`.
  """
  @spec merge(map(), map()) :: map()
  def merge(env, other) do
    Map.merge(env, other)
  end

  @doc """
  Marks a name as a local variable (not a module-level function).
  This affects how the type checker emits code for function calls.
  """
  @spec mark_local(map(), atom()) :: map()
  def mark_local(env, name) when is_atom(name) do
    locals = Map.get(env, :__local_vars__, MapSet.new())
    Map.put(env, :__local_vars__, MapSet.put(locals, name))
  end

  @doc """
  Checks if a name is a local variable.
  """
  @spec local?(map(), atom()) :: boolean()
  def local?(env, name) when is_atom(name) do
    locals = Map.get(env, :__local_vars__, MapSet.new())
    MapSet.member?(locals, name)
  end

  @doc """
  Checks if a name is defined in the environment.
  """
  @spec has?(map(), atom()) :: boolean()
  def has?(env, name) when is_atom(name) do
    Map.has_key?(env, name)
  end
end
