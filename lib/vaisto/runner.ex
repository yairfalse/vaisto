defmodule Vaisto.Runner do
  @moduledoc """
  The execution engine for Vaisto. Handles module loading and
  process interoperability with Elixir/Erlang.

  This is the bridge that allows Elixir applications to:
  - Load compiled Vaisto modules at runtime
  - Start Vaisto processes (GenServers)
  - Call Vaisto functions directly
  - Send messages to Vaisto processes

  Compilation uses verified Liquid Core and native BEAM lowering.

  ## Example

      # Compile and load a Vaisto module
      source = "(defn double [x :int] :int (* x 2))"
      {:ok, mod} = Vaisto.Runner.compile_and_load(source, :MyMath)

      # Call the function
      result = Vaisto.Runner.call(mod, :double, [21])
      # => 42

  ## Process Example

      source = \"""
      (process counter 0
        [:inc (+ state 1)]
        [:get state])
      \"""
      {:ok, _} = Vaisto.Runner.compile_and_load(source, :CounterApp)

      # Start the process
      {:ok, pid} = Vaisto.Runner.spawn_process(Counter, 0)

      # Interact with it
      Vaisto.Runner.send_msg(pid, :inc)  # => 1
      Vaisto.Runner.send_msg(pid, :get)  # => 1
  """

  @doc """
  Compiles Vaisto source code and loads it into the VM.

  ## Options

  Options are forwarded to `Vaisto.Compilation` (prelude, interfaces, search paths and solver settings).

  Returns {:ok, module_name} on success, {:error, reason} on failure.
  """
  def compile_and_load(source, module_name \\ :VaistoModule, opts \\ [])

  def compile_and_load(source, module_name, opts) when is_binary(source) do
    case Vaisto.Compilation.compile(source, module_name, Keyword.put(opts, :format_errors, false)) do
      {:ok, actual_name, bytecode} when is_binary(bytecode) -> {:ok, actual_name}
      {:error, reason} ->
        {:error, normalize_error(reason)}
    end
  end

  @doc """
  Loads a pre-compiled Vaisto module (bytecode) into the BEAM VM.

  This is useful when you want to cache compiled modules or
  distribute pre-compiled Vaisto code.
  """
  def load_module(module_name, binary) when is_atom(module_name) and is_binary(binary) do
    case :code.load_binary(module_name, ~c"vaisto_source", binary) do
      {:module, ^module_name} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Calls a function in a loaded Vaisto module.

  ## Examples

      Vaisto.Runner.call(MyMath, :double, [21])
      # => 42

      Vaisto.Runner.call(MyMath, :main)
      # => calls main/0
  """
  def call(module, function, args \\ []) when is_atom(module) and is_atom(function) do
    apply(module, function, args)
  end

  @doc """
  Starts a Vaisto process (GenServer) and returns its PID.

  The module should have been compiled from a Vaisto `(process ...)` form.
  """
  def spawn_process(module, init_arg) when is_atom(module) do
    module.start_link(init_arg)
  end

  @doc """
  Sends a synchronous message to a Vaisto process and waits for a reply.

  Uses GenServer.call under the hood.
  """
  def send_msg(pid, msg) when is_pid(pid) do
    GenServer.call(pid, msg)
  end

  @doc """
  Sends an asynchronous message to a Vaisto process (fire and forget).

  Uses GenServer.cast under the hood.
  """
  def send_async(pid, msg) when is_pid(pid) do
    GenServer.cast(pid, msg)
  end

  @doc """
  Runs a Vaisto expression and returns the result.

  This compiles the expression to a temporary module, executes it, and returns
  the result. Useful for REPL-style interaction.

  Note: This executes type-checked Vaisto code, not arbitrary Elixir code.
  The type system ensures safety before execution.

  ## Options

  Options are forwarded to `Vaisto.Compilation` (prelude, interfaces, search paths and solver settings).

  ## Example

      Vaisto.Runner.run("(+ 1 2)")
      # => {:ok, 3}

      Vaisto.Runner.run("(let [x 10] (* x x))")
      # => {:ok, 100}
  """
  def run(source, opts \\ []) when is_binary(source) do
    Vaisto.Compilation.run(source, Keyword.put(opts, :format_errors, false))
  end

  @doc """
  Checks if a Vaisto module is loaded in the VM.
  """
  def module_loaded?(module_name) when is_atom(module_name) do
    :code.is_loaded(module_name) != false
  end

  @doc """
  Unloads a Vaisto module from the VM.
  """
  def unload_module(module_name) when is_atom(module_name) do
    :code.purge(module_name)
    :code.delete(module_name)
    :ok
  end

  # Normalize error format for API compatibility
  defp normalize_error([]), do: %Vaisto.Error{message: "unknown error"}
  defp normalize_error(errors) when is_list(errors), do: Vaisto.Error.normalize(hd(errors))
  defp normalize_error(%Vaisto.Error{} = err), do: err
  defp normalize_error(err), do: Vaisto.Error.normalize(err)
end
