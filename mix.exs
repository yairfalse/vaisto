defmodule Vaisto.MixProject do
  use Mix.Project

  def project do
    [
      app: :vaisto,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript(),
      description: "A statically-typed language for distributed systems on BEAM"
    ]
  end

  defp escript do
    [main_module: Vaisto.CLI, name: "vaistoc"]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:jason, "~> 1.4"}
    ]
  end
end
