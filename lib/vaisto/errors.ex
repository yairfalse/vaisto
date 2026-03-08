defmodule Vaisto.Errors do
  @moduledoc """
  Error constructors for all Vaisto compiler errors.
  """

  alias Vaisto.Error

  # ============================================================================
  # Type Errors
  # ============================================================================

  @doc "Type mismatch between expected and actual types"
  def type_mismatch(expected, actual, opts \\ []) do
    Error.new("type mismatch",
      Keyword.merge(opts, [expected: expected, actual: actual])
    )
  end

  @doc "List elements have inconsistent types"
  def list_type_mismatch(expected, actual, opts \\ []) do
    Error.new("list elements must have the same type",
      Keyword.merge(opts, [expected: expected, actual: actual])
    )
  end

  @doc "If/match branches have different types"
  def branch_type_mismatch(branch1_type, branch2_type, opts \\ []) do
    Error.new("branch types must match",
      Keyword.merge(opts, [
        expected: branch1_type,
        actual: branch2_type,
        note: "all branches of a conditional must return the same type"
      ])
    )
  end

  @doc "Return type doesn't match declared type"
  def return_type_mismatch(declared, inferred, opts \\ []) do
    Error.new("return type mismatch",
      Keyword.merge(opts, [
        expected: declared,
        actual: inferred,
        hint: "the function body returns a different type than declared"
      ])
    )
  end

  @doc "Wrong number of arguments to function"
  def arity_mismatch(func_name, expected, actual, opts \\ []) do
    Error.new("wrong number of arguments",
      Keyword.merge(opts, [
        note: "`#{func_name}` takes #{expected} argument(s), but #{actual} were provided"
      ])
    )
  end

  @doc "cons type mismatch between element and list"
  def cons_type_mismatch(elem_type, list_type, opts \\ []) do
    Error.new("cons type mismatch",
      Keyword.merge(opts, [
        expected: list_type,
        actual: elem_type,
        hint: "element type must match list element type"
      ])
    )
  end

  @doc "Operation expects a list"
  def not_a_list(operation, actual, opts \\ []) do
    Error.new("expected a list",
      Keyword.merge(opts, [
        expected: {:list, :any},
        actual: actual,
        note: "`#{operation}` operates on lists"
      ])
    )
  end

  @doc "Operation expects a function"
  def not_a_function(operation, actual, opts \\ []) do
    Error.new("expected a function",
      Keyword.merge(opts, [
        expected: {:fn, [:any], :any},
        actual: actual,
        note: "`#{operation}` requires a function argument"
      ])
    )
  end

  @doc "map/filter function has wrong arity"
  def mapper_arity(operation, expected, actual, opts \\ []) do
    Error.new("#{operation} function has wrong arity",
      Keyword.merge(opts, [
        note: "`#{operation}` function must take exactly #{expected} argument(s), got #{actual}"
      ])
    )
  end

  @doc "Calling a non-function value"
  def non_function_call(actual, opts \\ []) do
    Error.new("cannot call non-function",
      Keyword.merge(opts, [
        actual: actual,
        note: "only functions can be called"
      ])
    )
  end

  @doc "Field access failed (row unification)"
  def field_access_error(field, actual, opts \\ []) do
    Error.new("field access error",
      Keyword.merge(opts, [
        actual: actual,
        note: "cannot access field `#{field}` on this type"
      ])
    )
  end

  @doc "filter predicate must return bool"
  def predicate_not_bool(actual, opts \\ []) do
    Error.new("predicate must return Bool",
      Keyword.merge(opts, [
        expected: :bool,
        actual: actual,
        hint: "filter predicates must return true or false"
      ])
    )
  end

  # ============================================================================
  # Unification Errors
  # ============================================================================

  @doc "Generic unification failure between two incompatible types"
  def unification_error(type1, type2, opts \\ []) do
    Error.new("cannot unify types",
      Keyword.merge(opts, [expected: type1, actual: type2])
    )
  end

  @doc "Function arity mismatch during unification"
  def function_arity_mismatch_unify(arity1, arity2, opts \\ []) do
    Error.new("function arity mismatch",
      Keyword.merge(opts, [
        note: "expected #{arity1} argument(s), got #{arity2}"
      ])
    )
  end

  @doc "Record name mismatch during unification"
  def record_name_mismatch(name1, name2, opts \\ []) do
    Error.new("cannot unify records",
      Keyword.merge(opts, [
        expected: {:record, name1, []},
        actual: {:record, name2, []},
        note: "record `#{name1}` is not compatible with `#{name2}`"
      ])
    )
  end

  @doc "Sum type name mismatch during unification"
  def sum_name_mismatch(name1, name2, opts \\ []) do
    Error.new("cannot unify sum types",
      Keyword.merge(opts, [
        expected: {:sum, name1, []},
        actual: {:sum, name2, []},
        note: "`#{name1}` is not compatible with `#{name2}`"
      ])
    )
  end

  @doc "Tuple size mismatch during unification"
  def tuple_size_mismatch(size1, size2, opts \\ []) do
    Error.new("tuple size mismatch",
      Keyword.merge(opts, [
        note: "expected #{size1}-element tuple, got #{size2}-element tuple"
      ])
    )
  end

  @doc "Occurs check failure (infinite type)"
  def occurs_check_error(id, type, opts \\ []) do
    type_str = Vaisto.TypeSystem.Core.format_type(type)
    Error.new("infinite type",
      Keyword.merge(opts, [
        note: "type variable t#{id} occurs in #{type_str}"
      ])
    )
  end

  @doc "Row occurs check failure (infinite row type)"
  def row_occurs_check_error(id, type, opts \\ []) do
    type_str = Vaisto.TypeSystem.Core.format_type(type)
    Error.new("infinite row type",
      Keyword.merge(opts, [
        note: "row variable r#{id} occurs in #{type_str}"
      ])
    )
  end

  @doc "Record field mismatch during unification"
  def record_field_mismatch(opts \\ []) do
    Error.new("record field mismatch",
      Keyword.merge(opts, [
        note: "records have different fields"
      ])
    )
  end

  @doc "Variant constructor mismatch during unification"
  def variant_mismatch(name1, name2, opts \\ []) do
    Error.new("variant mismatch",
      Keyword.merge(opts, [
        note: "variant `#{name1}` vs `#{name2}`"
      ])
    )
  end

  @doc "Variant count mismatch during unification"
  def variant_count_mismatch(opts \\ []) do
    Error.new("variant count mismatch", opts)
  end

  @doc "Row field mismatch during row unification"
  def row_field_mismatch(detail, opts \\ []) do
    Error.new("row field mismatch",
      Keyword.merge(opts, [note: detail])
    )
  end

  # ============================================================================
  # Name Resolution Errors
  # ============================================================================

  @doc "Variable not defined in scope"
  def undefined_variable(name, known_names_or_opts \\ [])

  def undefined_variable(name, known_names) when is_list(known_names) and known_names != [] and is_atom(hd(known_names)) do
    hint = suggest_name(name, known_names)
    Error.new("undefined variable",
      note: "`#{name}` is not defined in this scope",
      hint: hint
    )
  end

  def undefined_variable(name, opts) do
    Error.new("undefined variable",
      Keyword.merge(opts, [
        note: "`#{name}` is not defined in this scope"
      ])
    )
  end

  @doc "Prompt output does not satisfy the extract target"
  def prompt_output_mismatch(prompt_name, prompt_output_type, extract_target, missing_or_mistyped_fields, opts \\ []) do
    detail_lines =
      missing_or_mistyped_fields
      |> Enum.map(fn
        {:missing, field, expected_type} ->
          "missing field: #{field} : #{Vaisto.TypeSystem.Core.format_type(expected_type)}"

        {:mistyped, field, expected_type, actual_type} ->
          "field `#{field}` has type #{Vaisto.TypeSystem.Core.format_type(actual_type)}, expected #{Vaisto.TypeSystem.Core.format_type(expected_type)}"
      end)

    note =
      [
        "prompt `#{prompt_name}` output #{Vaisto.TypeSystem.Core.format_type(prompt_output_type)} does not satisfy extract target #{Vaisto.TypeSystem.Core.format_type(extract_target)}"
        | detail_lines
      ]
      |> Enum.join("\n  note: ")

    Error.new("prompt output type mismatch",
      Keyword.merge(opts, [
        expected: extract_target,
        actual: prompt_output_type,
        note: note
      ])
    )
  end

  @doc "Prompt not defined in scope"
  def undefined_prompt(name, known_names_or_opts \\ [])

  def undefined_prompt(name, known_names) when is_list(known_names) and known_names != [] and is_atom(hd(known_names)) do
    hint = suggest_name(name, known_names)

    Error.new("undefined prompt",
      note: "prompt `#{name}` is not defined",
      hint: hint
    )
  end

  def undefined_prompt(name, opts) do
    Error.new("undefined prompt",
      Keyword.merge(opts, [
        note: "prompt `#{name}` is not defined"
      ])
    )
  end

  @doc "Prompt is missing a template required for code generation"
  def prompt_missing_template(name, opts \\ []) do
    Error.new("prompt missing template",
      Keyword.merge(opts, [
        note: "prompt `#{name}` requires a :template clause before it can be emitted"
      ])
    )
  end

  @doc "Function not found"
  def unknown_function(name, known_names_or_opts \\ [])

  def unknown_function(name, known_names) when is_list(known_names) and known_names != [] and is_atom(hd(known_names)) do
    name_str = format_function_name(name)
    hint = suggest_name(name, known_names) || suggest_function(name)
    Error.new("unknown function",
      note: "`#{name_str}` is not defined",
      hint: hint
    )
  end

  def unknown_function(name, opts) do
    name_str = format_function_name(name)
    Error.new("unknown function",
      Keyword.merge(opts, [
        note: "`#{name_str}` is not defined",
        hint: suggest_function(name)
      ])
    )
  end

  @doc "Type not found"
  def unknown_type(name, opts \\ []) do
    Error.new("unknown type",
      Keyword.merge(opts, [
        note: "type `#{name}` is not defined"
      ])
    )
  end

  @doc "Process not defined"
  def unknown_process(name, known_names_or_opts \\ [])

  def unknown_process(name, known_names) when is_list(known_names) and known_names != [] and is_atom(hd(known_names)) do
    hint = suggest_name(name, known_names)
    Error.new("unknown process",
      note: "process `#{name}` is not defined in this module",
      hint: hint
    )
  end

  def unknown_process(name, opts) do
    Error.new("unknown process",
      Keyword.merge(opts, [
        note: "process `#{name}` is not defined in this module"
      ])
    )
  end

  # ============================================================================
  # Syntax Errors
  # ============================================================================

  @doc "Invalid defn syntax"
  def invalid_defn_syntax(opts \\ []) do
    Error.new("invalid function definition",
      Keyword.merge(opts, [
        hint: "expected (defn name [params] body) or (defn name [params] :type body)"
      ])
    )
  end

  @doc "Parse error"
  def parse_error(message, opts \\ []) do
    Error.new(message, opts)
  end

  @doc "Unknown expression type"
  def unknown_expression(expr, opts \\ []) do
    Error.new("unknown expression",
      Keyword.merge(opts, [
        note: "cannot type check: #{inspect(expr)}"
      ])
    )
  end

  @doc "Non-exhaustive pattern match on sum type"
  def non_exhaustive_sum(type_name, missing_variants, opts \\ []) do
    missing_list = missing_variants |> Enum.map(&to_string/1) |> Enum.join(", ")
    Error.new("non-exhaustive pattern match",
      Keyword.merge(opts, [
        note: "match on `#{type_name}` does not cover all variants",
        hint: "missing variants: #{missing_list}"
      ])
    )
  end

  @doc "Non-exhaustive pattern match on Bool"
  def non_exhaustive_bool(missing, opts \\ []) do
    Error.new("non-exhaustive pattern match on Bool",
      Keyword.merge(opts, [hint: "missing: #{missing}"]))
  end

  @doc "Non-exhaustive pattern match on result tuples"
  def non_exhaustive_result(missing_tags, opts \\ []) do
    missing_str = missing_tags |> Enum.map(&":#{&1}") |> Enum.join(", ")
    Error.new("non-exhaustive pattern match",
      Keyword.merge(opts, [
        note: "result-like match is not exhaustive",
        hint: "missing patterns for: #{missing_str}"
      ])
    )
  end

  @doc "Unknown supervision strategy"
  def unknown_supervision_strategy(strategy, opts \\ []) do
    Error.new("unknown supervision strategy",
      Keyword.merge(opts, [
        note: "`#{inspect(strategy)}` is not a valid strategy",
        hint: "valid strategies are :one_for_one, :all_for_one, :rest_for_one"
      ])
    )
  end

  @doc "Extern function lookup failed"
  def extern_not_a_function(mod, func, actual, opts \\ []) do
    Error.new("extern is not a function",
      Keyword.merge(opts, [
        note: "`#{mod}:#{func}` resolved to #{inspect(actual)}, not a function type"
      ])
    )
  end

  # ============================================================================
  # Process/Concurrency Errors
  # ============================================================================

  @doc "Process doesn't accept this message type"
  def invalid_message(process_name, message, accepted, opts \\ []) do
    accepted_str = accepted |> Enum.map(&":#{&1}") |> Enum.join(", ")
    Error.new("invalid message type",
      Keyword.merge(opts, [
        note: "process `#{process_name}` does not accept `:#{message}`",
        hint: "accepted messages: #{accepted_str}"
      ])
    )
  end

  @doc "Can only send to PIDs"
  def send_to_non_pid(actual, opts \\ []) do
    Error.new("cannot send to non-pid",
      Keyword.merge(opts, [
        actual: actual,
        note: "the `!` operator requires a PID as the first argument"
      ])
    )
  end

  # ============================================================================
  # Type Class Errors
  # ============================================================================

  @doc "Reference to an undefined type class"
  def unknown_type_class(name, known_classes \\ []) do
    hint = suggest_name(name, known_classes)
    Error.new("unknown type class",
      note: "`#{name}` is not defined as a type class",
      hint: hint
    )
  end

  @doc "Reference to an undefined type class in a constraint"
  def unknown_type_class_in_constraint(name, known_classes \\ []) do
    hint = case suggest_name(name, known_classes) do
      nil -> "constraints must reference defined type classes"
      suggestion -> suggestion
    end
    Error.new("unknown type class in constraint",
      note: "`#{name}` is not defined as a type class",
      hint: hint
    )
  end

  @doc "Type class instance is missing required methods"
  def missing_instance_methods(class, type, missing) do
    missing_str = missing |> Enum.map(&to_string/1) |> Enum.join(", ")
    Error.new("missing instance methods",
      note: "instance `#{class}` for `#{format_instance_type(type)}` is missing methods: #{missing_str}",
      hint: "implement all required methods or provide defaults in the class definition"
    )
  end

  @doc "No type class instance exists for a given type"
  def no_instance_for_type(class, type, opts \\ []) do
    type_str = Vaisto.TypeFormatter.format(type)
    note = case Keyword.get(opts, :required_by) do
      nil -> nil
      ctx -> "required by constrained instance `#{ctx}`"
    end
    Error.new("no instance of `#{class}` for type `#{type_str}`",
      note: note
    )
  end

  @doc "Constraint resolution exceeded maximum depth"
  def constraint_depth_exceeded do
    Error.new("constraint resolution depth exceeded",
      note: "possible infinite chain of constrained instances",
      hint: "check for circular constraints between type class instances"
    )
  end

  @doc "Cannot derive Show for a sum type with fields"
  def derive_show_has_fields(type) do
    Error.new("cannot derive Show",
      note: "`#{type}` has variants with fields",
      hint: "write a manual `(instance Show #{type} ...)` instead"
    )
  end

  @doc "Cannot derive Show for a record type"
  def derive_show_record(type) do
    Error.new("cannot derive Show",
      note: "record type `#{type}` requires a manual Show instance",
      hint: "write a manual `(instance Show #{type} ...)` instead"
    )
  end

  @doc "Derive is not supported for this type class"
  def derive_not_supported(class) do
    Error.new("cannot derive `#{class}`",
      note: "only Eq and Show are derivable"
    )
  end

  # ============================================================================
  # Manifest Errors
  # ============================================================================

  @doc "Invalid TOML in vaisto.toml"
  def manifest_parse_error(reason) do
    Error.new("invalid vaisto.toml", note: "#{inspect(reason)}")
  end

  @doc "Cannot read vaisto.toml"
  def manifest_read_error(reason) do
    Error.new("cannot read vaisto.toml", note: "#{reason}")
  end

  @doc "Missing package name in manifest"
  def manifest_missing_name do
    Error.new("missing package name",
      note: "vaisto.toml: missing [package] name"
    )
  end

  @doc "Package name must be a string"
  def manifest_name_not_string do
    Error.new("package name must be a string",
      note: "vaisto.toml: name must be a string"
    )
  end

  @doc "Package name is required"
  def manifest_name_required do
    Error.new("package name is required")
  end

  @doc "Invalid package name format"
  def manifest_name_format(name) do
    Error.new("invalid package name",
      note: "package name `#{name}` must be lowercase kebab-case (e.g. \"my-package\")"
    )
  end

  @doc "Package name too long"
  def manifest_name_too_long(name) do
    Error.new("package name too long",
      note: "package name `#{name}` is too long (max 64 characters)"
    )
  end

  @doc "Invalid version format"
  def manifest_version_format(version) do
    Error.new("invalid version",
      note: "version `#{version}` must follow semver (e.g. \"1.0.0\")"
    )
  end

  @doc "Source directory not found"
  def manifest_missing_source_dir(dirs) when is_list(dirs) do
    detail = case dirs do
      [dir] -> "source directory `#{dir}` does not exist"
      dirs -> "source directories do not exist: #{Enum.join(dirs, ", ")}"
    end
    Error.new("source directory not found", note: detail)
  end

  @doc "Invalid dependency spec"
  def manifest_invalid_dep_spec(name, spec) do
    Error.new("invalid dependency spec",
      note: "invalid dependency spec for `#{name}`: #{inspect(spec)}"
    )
  end

  # ============================================================================
  # Interface Errors
  # ============================================================================

  @doc "Corrupt interface file"
  def corrupt_interface(path) do
    Error.new("corrupt interface file", note: path)
  end

  @doc "Cannot read interface file"
  def interface_read_error(path, reason) do
    Error.new("cannot read interface file",
      note: "Cannot read #{path}: #{reason}"
    )
  end

  @doc "Interface not found for module"
  def interface_not_found(module_name) do
    Error.new("interface not found",
      note: "Interface not found for #{module_name}"
    )
  end

  @doc "Incompatible interface version"
  def interface_version_mismatch(found, expected) do
    Error.new("incompatible interface version",
      note: "found version #{found}, expected #{expected}"
    )
  end

  @doc "Invalid interface format"
  def interface_invalid_format do
    Error.new("invalid interface format")
  end

  # ============================================================================
  # LSP Errors
  # ============================================================================

  @doc "Invalid identifier for rename"
  def invalid_rename_target(name) do
    Error.new("invalid identifier",
      note: "Invalid identifier: #{name}"
    )
  end

  @doc "No symbol at cursor position"
  def no_symbol_at_position do
    Error.new("no symbol at position")
  end

  # ============================================================================
  # CLI Errors
  # ============================================================================

  @doc "CLI argument validation error"
  def cli_error(message, opts \\ []) do
    Error.new(message, opts)
  end

  # ============================================================================
  # Compilation Errors
  # ============================================================================

  @doc "Compilation error (wraps exception messages)"
  def compilation_error(note, opts \\ []) do
    Error.new("compilation error", Keyword.merge(opts, [note: note]))
  end

  @doc "Cannot read source file"
  def source_read_error(path, reason) do
    Error.new("cannot read source file",
      note: "Cannot read #{path}: #{reason}"
    )
  end

  @doc "Runtime error during evaluation"
  def runtime_error(exception_msg) do
    Error.new("runtime error", note: exception_msg)
  end

  @doc "Unexpected result from compilation"
  def unexpected_result(value) do
    Error.new("unexpected result", note: inspect(value))
  end

  @doc "BEAM compilation failed"
  def beam_compilation_failed(detail, opts \\ []) do
    Error.new("BEAM compilation failed", Keyword.merge(opts, [note: detail]))
  end

  @doc "Unknown backend specified"
  def unknown_backend(backend, opts \\ []) do
    Error.new("unknown backend",
      Keyword.merge(opts, [
        note: "#{inspect(backend)} is not valid",
        hint: "use :core or :elixir"
      ])
    )
  end

  # ============================================================================
  # Build Errors
  # ============================================================================

  @doc "No manifest found in directory"
  def no_manifest(dir, opts \\ []) do
    Error.new("no vaisto.toml found in #{dir}", opts)
  end

  @doc "Dependency resolution error"
  def dependency_error(name, reason, opts \\ []) do
    Error.new("dependency `#{name}`: #{reason}", opts)
  end

  @doc "Module namespace violation"
  def namespace_violation(module, prefix, package) do
    Error.new("namespace violation",
      note: "module `#{module}` must start with `#{prefix}` (package: #{package})"
    )
  end

  @doc "No source files found"
  def no_source_files(dir \\ nil, opts \\ []) do
    msg = if dir, do: "no .va files found in #{dir}", else: "no .va files found in source directories"
    Error.new(msg, opts)
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp suggest_function(name) when is_atom(name) do
    common = ~w(map filter fold head tail cons empty? length if let match defn deftype)a
    suggest_name(name, common)
  end
  defp suggest_function(_), do: nil

  defp suggest_name(name, candidates) when is_atom(name) and is_list(candidates) do
    name_str = Atom.to_string(name)
    result = Enum.find(candidates, fn candidate ->
      String.jaro_distance(name_str, Atom.to_string(candidate)) > 0.75
    end)
    if result, do: "did you mean `#{result}`?", else: nil
  end
  defp suggest_name(_, _), do: nil

  defp format_function_name({:module_path, parts}) when is_list(parts) do
    Enum.join(parts, ".")
  end
  defp format_function_name(name) when is_atom(name), do: Atom.to_string(name)
  defp format_function_name(name) when is_binary(name), do: name
  defp format_function_name(name), do: inspect(name)

  defp format_instance_type(type) when is_atom(type), do: Atom.to_string(type)
  defp format_instance_type(type), do: Vaisto.TypeFormatter.format(type)
end
