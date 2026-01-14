defmodule Vaisto.MixProject do
  use Mix.Project

  def project do
    [
      app: :vaisto,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "A statically-typed Scheme for the BEAM"
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    []
  end
end
