defmodule Vaisto.Package.Namespace do
  @moduledoc """
  Package name to module namespace mapping.

  Converts kebab-case package names to PascalCase module prefixes.
  This is how packages get their own namespace: a package named "json-parser"
  owns the `JsonParser.*` module namespace.

  ## Examples

      iex> Namespace.to_prefix("json")
      "Json"

      iex> Namespace.to_prefix("json-parser")
      "JsonParser"

      iex> Namespace.to_source_root("../json", "json")
      [{"../json/src/", ""}]
  """

  @doc """
  Convert a kebab-case package name to a PascalCase module prefix.

  ## Examples

      iex> Namespace.to_prefix("json")
      "Json"

      iex> Namespace.to_prefix("json-parser")
      "JsonParser"

      iex> Namespace.to_prefix("http-server")
      "HttpServer"
  """
  @spec to_prefix(String.t()) :: String.t()
  def to_prefix(package_name) do
    package_name
    |> String.split("-")
    |> Enum.map(&capitalize/1)
    |> Enum.join()
  end

  @doc """
  Build a source root tuple for a dependency.

  Given a dependency's path and package name, returns the `{path_prefix, module_prefix}`
  tuple that `ModuleNaming` uses to map files to module names.

  Uses the first source directory from the dependency's manifest, defaulting to "src/".

  ## Examples

      iex> Namespace.to_source_root("../json", "json")
      [{"../json/src/", ""}]

      iex> Namespace.to_source_root("../tcp", "tcp", ["lib"])
      [{"../tcp/lib/", ""}]
  """
  @spec to_source_root(String.t(), String.t(), [String.t()]) :: [{String.t(), String.t()}]
  def to_source_root(dep_path, _package_name, source_dirs \\ ["src"]) do
    # No prefix: the files inside the dep are expected to already be correctly namespaced.
    # e.g. string-utils/src/StringUtils.va → StringUtils (root module)
    #      string-utils/src/StringUtils/Parser.va → StringUtils.Parser
    # Any namespace enforcement must be handled by callers (e.g. via `validate_module/2`).
    Enum.map(source_dirs, fn dir ->
      path = Path.join(dep_path, dir)
      path = if String.ends_with?(path, "/"), do: path, else: path <> "/"
      {path, ""}
    end)
  end

  @doc """
  Validate that a module name belongs to the expected package namespace.

  Returns `:ok` if the module name starts with the package prefix (or equals it),
  `{:error, message}` otherwise.

  ## Examples

      iex> Namespace.validate_module("Json.Parser", "json")
      :ok

      iex> Namespace.validate_module("Json", "json")
      :ok

      iex> Namespace.validate_module("Foo.Bar", "json")
      {:error, "module `Foo.Bar` must start with `Json` (package: json)"}
  """
  @spec validate_module(String.t(), String.t()) :: :ok | {:error, String.t()}
  def validate_module(module_name, package_name) do
    expected = to_prefix(package_name)
    clean = module_name |> to_string() |> String.replace_prefix("Elixir.", "")

    if clean == expected or String.starts_with?(clean, expected <> ".") do
      :ok
    else
      {:error, "module `#{clean}` must start with `#{expected}` (package: #{package_name})"}
    end
  end

  defp capitalize(<<first::utf8, rest::binary>>), do: String.upcase(<<first::utf8>>) <> rest
  defp capitalize(""), do: ""
end
