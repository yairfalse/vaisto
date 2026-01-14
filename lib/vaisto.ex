defmodule Vaisto do
  @moduledoc """
  Vaisto - A statically-typed Scheme for the BEAM.
  
  Finnish for "intuition" — the flash of recognition 
  when the right structure becomes visible.
  """

  alias Vaisto.{Parser, TypeChecker, CoreEmitter}

  @doc """
  Compile a .va file to BEAM bytecode.
  """
  def compile(source_path) do
    source_path
    |> File.read!()
    |> compile_string()
    |> write_beam_file(source_path)
  end

  @doc """
  Compile a Vaisto source string.
  """
  def compile_string(source) do
    source
    |> Parser.parse()
    |> TypeChecker.check!()
    |> CoreEmitter.emit()
  end

  @doc """
  Parse and type-check without emitting (for REPL use).
  """
  def check(source) do
    source
    |> Parser.parse()
    |> TypeChecker.check()
  end

  defp write_beam_file(binary, path) do
    out_path = String.replace(path, ".va", ".beam")
    File.write!(out_path, binary)
    IO.puts("✨ Compiled: #{out_path}")
    {:ok, out_path}
  end
end
