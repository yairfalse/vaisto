defmodule Vaisto.Package.Manifest do
  @moduledoc """
  Parse and validate `vaisto.toml` manifests.

  A manifest describes a Vaisto package: its name, version, dependencies,
  and build configuration. The manifest is optional — projects without one
  continue to work exactly as before.

  ## Minimal manifest

      [package]
      name = "my-app"

  Everything else has sensible defaults: version "0.1.0", edition "2026",
  source in `src/`, output in `build/`.

  ## Full manifest

      [package]
      name = "http-server"
      version = "0.3.1"
      edition = "2026"
      description = "Type-safe HTTP server for BEAM"
      license = "MIT"

      [dependencies]
      json = "~> 1.2"
      tcp = { path = "../tcp" }

      [dev-dependencies]
      test-helpers = "~> 0.1"

      [build]
      source-dirs = ["src"]
      output-dir = "build"
  """

  defstruct [
    :name,
    :description,
    :license,
    version: "0.1.0",
    edition: "2026",
    source_dirs: ["src"],
    output_dir: "build",
    dependencies: %{},
    dev_dependencies: %{}
  ]

  @type dep_spec :: String.t() | %{path: String.t()}

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          edition: String.t(),
          description: String.t() | nil,
          license: String.t() | nil,
          source_dirs: [String.t()],
          output_dir: String.t(),
          dependencies: %{String.t() => dep_spec()},
          dev_dependencies: %{String.t() => dep_spec()}
        }

  @manifest_file "vaisto.toml"

  @doc """
  Load and validate a manifest from a project directory.

  Returns `{:ok, manifest}` if `vaisto.toml` exists and is valid,
  `{:error, reason}` if it exists but is invalid,
  `:none` if no manifest file exists.
  """
  @spec load(String.t()) :: {:ok, t()} | {:error, String.t()} | :none
  def load(dir) do
    path = Path.join(dir, @manifest_file)

    case File.read(path) do
      {:ok, content} ->
        case Toml.decode(content) do
          {:ok, toml} ->
            parse(toml, dir)

          {:error, reason} ->
            {:error, "invalid vaisto.toml: #{inspect(reason)}"}
        end

      {:error, :enoent} ->
        :none

      {:error, reason} ->
        {:error, "cannot read vaisto.toml: #{reason}"}
    end
  end

  @doc """
  Check if a manifest file exists in the given directory.
  """
  @spec exists?(String.t()) :: boolean()
  def exists?(dir) do
    dir |> Path.join(@manifest_file) |> File.exists?()
  end

  @doc """
  Validate a parsed manifest.

  Checks: name is kebab-case, version looks like semver, source dirs exist.
  """
  @spec validate(t(), String.t()) :: :ok | {:error, String.t()}
  def validate(%__MODULE__{} = manifest, project_dir) do
    with :ok <- validate_name(manifest.name),
         :ok <- validate_version(manifest.version),
         :ok <- validate_source_dirs(manifest.source_dirs, project_dir),
         :ok <- validate_deps(manifest.dependencies) do
      :ok
    end
  end

  @doc """
  Generate the contents of a minimal `vaisto.toml` for a new project.
  """
  @spec generate(String.t()) :: String.t()
  def generate(package_name) do
    """
    [package]
    name = "#{package_name}"
    version = "0.1.0"
    """
  end

  # --- Parsing ---

  defp parse(toml, dir) do
    with {:ok, name} <- extract_name(toml),
         package = Map.get(toml, "package", %{}),
         build = Map.get(toml, "build", %{}) do
      manifest = %__MODULE__{
        name: name,
        version: Map.get(package, "version", "0.1.0"),
        edition: Map.get(package, "edition", "2026"),
        description: Map.get(package, "description"),
        license: Map.get(package, "license"),
        source_dirs: Map.get(build, "source-dirs", ["src"]),
        output_dir: Map.get(build, "output-dir", "build"),
        dependencies: parse_deps(Map.get(toml, "dependencies", %{})),
        dev_dependencies: parse_deps(Map.get(toml, "dev-dependencies", %{}))
      }

      case validate(manifest, dir) do
        :ok -> {:ok, manifest}
        {:error, _} = err -> err
      end
    end
  end

  defp extract_name(toml) do
    case get_in(toml, ["package", "name"]) do
      nil -> {:error, "vaisto.toml: missing [package] name"}
      name when is_binary(name) -> {:ok, name}
      _ -> {:error, "vaisto.toml: name must be a string"}
    end
  end

  defp parse_deps(deps) when is_map(deps) do
    Map.new(deps, fn
      {name, version} when is_binary(version) ->
        {name, version}

      {name, %{"path" => path}} ->
        {name, %{path: path}}

      {name, other} ->
        {name, {:invalid_dependency_spec, other}}
    end)
  end

  defp parse_deps(_), do: %{}

  # --- Validation ---

  @doc """
  Validate that a package name is lowercase kebab-case.
  """
  @spec validate_name(String.t() | nil) :: :ok | {:error, String.t()}
  def validate_name(nil), do: {:error, "package name is required"}

  def validate_name(name) do
    cond do
      not Regex.match?(~r/^[a-z][a-z0-9]*(-[a-z][a-z0-9]*)*$/, name) ->
        {:error, "package name `#{name}` must be lowercase kebab-case (e.g. \"my-package\")"}

      String.length(name) > 64 ->
        {:error, "package name `#{name}` is too long (max 64 characters)"}

      true ->
        :ok
    end
  end

  defp validate_version(version) do
    if Regex.match?(~r/^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$/, version) do
      :ok
    else
      {:error, "version `#{version}` must follow semver (e.g. \"1.0.0\")"}
    end
  end

  defp validate_source_dirs(dirs, project_dir) do
    missing =
      Enum.reject(dirs, fn dir ->
        Path.join(project_dir, dir) |> File.dir?()
      end)

    case missing do
      [] -> :ok
      [dir] -> {:error, "source directory `#{dir}` does not exist"}
      dirs -> {:error, "source directories do not exist: #{Enum.join(dirs, ", ")}"}
    end
  end

  defp validate_deps(deps) do
    Enum.reduce_while(Map.to_list(deps), :ok, fn
      {_name, version}, :ok when is_binary(version) ->
        {:cont, :ok}

      {_name, %{path: path}}, :ok when is_binary(path) ->
        {:cont, :ok}

      {name, spec}, :ok ->
        {:halt, {:error, "invalid dependency spec for `#{name}`: #{inspect(spec)}"}}
    end)
  end
end
