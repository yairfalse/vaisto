defmodule Vaisto do
  @moduledoc """
  Vaisto - A statically-typed Scheme for the BEAM.
  
  Finnish for "intuition" — the flash of recognition 
  when the right structure becomes visible.
  """

  alias Vaisto.Compilation

  @doc """
  Compile a .va file to BEAM bytecode.
  """
  def compile(source_path) do
    with {:ok, source} <- File.read(source_path),
         {:ok, _module, binary} <- compile_string(source),
         out_path = Path.rootname(source_path, ".va") <> ".beam",
         :ok <- File.write(out_path, binary) do
      IO.puts("Compiled: #{out_path}")
      {:ok, out_path}
    end
  end

  @doc """
  Compile a Vaisto source string.
  """
  def compile_string(source, module_name \\ :VaistoModule) do
    Compilation.compile_liquid(source, module_name)
  end

  @doc """
  Parse and type-check without emitting (for REPL use).
  """
  def check(source) do
    Compilation.check_liquid(source, :VaistoModule, format_errors: false)
  end
end
