defmodule Vaisto.LSP.Completion do
  @moduledoc """
  LSP completion provider for Vaisto.

  Provides intelligent code completion for:
  - Keywords (defn, deftype, let, if, match, etc.)
  - Built-in functions (+, -, *, /, list, head, tail, etc.)
  - Types (:int, :float, :string, :bool, etc.)
  - User-defined functions and types from the current document
  """

  alias Vaisto.Parser

  # LSP CompletionItemKind constants
  @kind_keyword 14
  @kind_function 3
  @kind_type 22
  @kind_variable 6
  @kind_operator 24

  # ============================================================================
  # Keywords
  # ============================================================================

  @keywords [
    {"defn", "Define a function", "(defn name [params] body)"},
    {"deftype", "Define a type", "(deftype Name (Constructor field1 field2))"},
    {"defval", "Define a value", "(defval name value)"},
    {"let", "Local binding", "(let [x value] body)"},
    {"if", "Conditional expression", "(if condition then-expr else-expr)"},
    {"match", "Pattern matching", "(match expr [pattern1 result1] [pattern2 result2])"},
    {"cond", "Multi-way conditional", "(cond [test1 result1] [test2 result2] [:else default])"},
    {"do", "Sequence of expressions", "(do expr1 expr2 ... result)"},
    {"fn", "Anonymous function", "(fn [x] body)"},
    {"process", "Define a process", "(process name initial-state :msg handler)"},
    {"spawn", "Spawn a process", "(spawn process-name initial-state)"},
    {"extern", "External function binding", "(extern :module :function [:arg-types] :return-type)"},
    {"import", "Import a module", "(import Module)"},
    {"ns", "Namespace declaration", "(ns MyModule)"}
  ]

  # ============================================================================
  # Built-in Functions
  # ============================================================================

  @builtins [
    # Arithmetic
    {"+", "Addition", "(+ a b) → num", [:int, :int], :int},
    {"-", "Subtraction", "(- a b) → num", [:int, :int], :int},
    {"*", "Multiplication", "(* a b) → num", [:int, :int], :int},
    {"/", "Division", "(/ a b) → float", [:int, :int], :float},

    # Comparison
    {"<", "Less than", "(< a b) → bool", [:int, :int], :bool},
    {">", "Greater than", "(> a b) → bool", [:int, :int], :bool},
    {"<=", "Less than or equal", "(<= a b) → bool", [:int, :int], :bool},
    {">=", "Greater than or equal", "(>= a b) → bool", [:int, :int], :bool},
    {"==", "Equality", "(== a b) → bool", [:any, :any], :bool},
    {"!=", "Inequality", "(!= a b) → bool", [:any, :any], :bool},

    # Boolean
    {"and", "Logical and", "(and a b) → bool", [:bool, :bool], :bool},
    {"or", "Logical or", "(or a b) → bool", [:bool, :bool], :bool},
    {"not", "Logical not", "(not a) → bool", [:bool], :bool},

    # List operations
    {"list", "Create a list", "(list a b c) → [a, b, c]", [:any], {:list, :any}},
    {"head", "First element", "(head xs) → element", [{:list, :any}], :any},
    {"tail", "Rest of list", "(tail xs) → list", [{:list, :any}], {:list, :any}},
    {"cons", "Prepend element", "(cons x xs) → list", [:any, {:list, :any}], {:list, :any}},
    {"length", "List length", "(length xs) → int", [{:list, :any}], :int},
    {"map", "Map function over list", "(map f xs) → list", [{:fn, [:any], :any}, {:list, :any}], {:list, :any}},
    {"filter", "Filter list", "(filter f xs) → list", [{:fn, [:any], :bool}, {:list, :any}], {:list, :any}},
    {"fold", "Fold/reduce list", "(fold f init xs) → value", [{:fn, [:any, :any], :any}, :any, {:list, :any}], :any},

    # String operations
    {"str", "String concatenation/conversion", "(str a b c) → string", [:any], :string},

    # Process operations
    {"!", "Send message (type-checked)", "(! pid msg) → msg", [:pid, :any], :any},
    {"!!", "Send message (unchecked)", "(!! pid msg) → msg", [:pid, :any], :any},

    # Tuple operations
    {"tuple", "Create a tuple", "(tuple a b c) → (a, b, c)", [:any], :tuple}
  ]

  # ============================================================================
  # Types
  # ============================================================================

  @types [
    {":int", "Integer type", "Whole numbers"},
    {":float", "Float type", "Floating-point numbers"},
    {":num", "Numeric type", "Int or float"},
    {":string", "String type", "Text/binary data"},
    {":bool", "Boolean type", "true or false"},
    {":atom", "Atom type", "Named constants like :ok, :error"},
    {":any", "Any type", "Matches any type (escape hatch)"},
    {":unit", "Unit type", "No meaningful value ()"},
    {":pid", "Process ID type", "Reference to a process"}
  ]

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Get completions for the given position in the source text.
  Returns a list of LSP CompletionItem objects.
  """
  def get_completions(text, line, col, _file) do
    # Get the current line and determine context
    lines = String.split(text, "\n")
    current_line = Enum.at(lines, line - 1, "")
    prefix = String.slice(current_line, 0, col - 1)

    context = determine_context(prefix)
    completions = get_completions_for_context(context, text)

    # Filter by prefix if there's a partial word
    partial = extract_partial_word(prefix)
    filter_completions(completions, partial)
  end

  # ============================================================================
  # Context Detection
  # ============================================================================

  defp determine_context(prefix) do
    trimmed = String.trim_trailing(prefix)

    cond do
      # After opening paren - suggest keywords and functions
      String.ends_with?(trimmed, "(") ->
        :after_paren

      # After colon - suggest types
      String.ends_with?(trimmed, ":") ->
        :type

      # In a type annotation position (after parameter name in defn)
      Regex.match?(~r/\[[\w\s:]+$/, trimmed) ->
        :type_annotation

      # Default - suggest everything contextually
      true ->
        :general
    end
  end

  defp get_completions_for_context(:after_paren, text) do
    keyword_completions() ++ builtin_completions() ++ user_function_completions(text)
  end

  defp get_completions_for_context(:type, _text) do
    type_completions()
  end

  defp get_completions_for_context(:type_annotation, _text) do
    type_completions()
  end

  defp get_completions_for_context(:general, text) do
    keyword_completions() ++ builtin_completions() ++ type_completions() ++ user_function_completions(text)
  end

  # ============================================================================
  # Completion Generators
  # ============================================================================

  defp keyword_completions do
    Enum.map(@keywords, fn {name, doc, detail} ->
      %{
        "label" => name,
        "kind" => @kind_keyword,
        "detail" => detail,
        "documentation" => %{
          "kind" => "markdown",
          "value" => doc
        },
        "insertText" => name
      }
    end)
  end

  defp builtin_completions do
    Enum.map(@builtins, fn {name, doc, detail, _args, _ret} ->
      %{
        "label" => name,
        "kind" => if(name in ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="], do: @kind_operator, else: @kind_function),
        "detail" => detail,
        "documentation" => %{
          "kind" => "markdown",
          "value" => doc
        },
        "insertText" => name
      }
    end)
  end

  defp type_completions do
    Enum.map(@types, fn {name, doc, detail} ->
      # Remove leading colon for insert if context already has one
      insert_text = String.trim_leading(name, ":")

      %{
        "label" => name,
        "kind" => @kind_type,
        "detail" => detail,
        "documentation" => %{
          "kind" => "markdown",
          "value" => doc
        },
        "insertText" => insert_text
      }
    end)
  end

  defp user_function_completions(text) do
    try do
      ast = Parser.parse(text)
      forms = if is_list(ast), do: ast, else: [ast]

      forms
      |> Enum.flat_map(&extract_definitions/1)
      |> Enum.map(fn {name, kind, detail} ->
        %{
          "label" => to_string(name),
          "kind" => kind,
          "detail" => detail,
          "insertText" => to_string(name)
        }
      end)
    rescue
      _ -> []
    end
  end

  defp extract_definitions({:defn, name, params, _body, _loc}) do
    arity = length(params)
    [{name, @kind_function, "function/#{arity}"}]
  end

  defp extract_definitions({:defn, name, params, _body, _ret, _loc}) do
    arity = length(params)
    [{name, @kind_function, "function/#{arity}"}]
  end

  defp extract_definitions({:defn_multi, name, arity, _clauses, _loc}) do
    [{name, @kind_function, "function/#{arity}"}]
  end

  defp extract_definitions({:deftype, name, {:sum, variants}, _loc}) do
    type_completion = {name, @kind_type, "sum type"}
    constructor_completions = Enum.map(variants, fn {ctor_name, args} ->
      arity = length(args)
      {ctor_name, @kind_function, "constructor/#{arity}"}
    end)
    [type_completion | constructor_completions]
  end

  defp extract_definitions({:deftype, name, {:record, _fields}, _loc}) do
    [{name, @kind_type, "record type"}]
  end

  defp extract_definitions({:defval, name, _expr, _loc}) do
    [{name, @kind_variable, "value"}]
  end

  defp extract_definitions({:process, name, _init, _handlers, _loc}) do
    [{name, @kind_function, "process"}]
  end

  defp extract_definitions({:extern, _mod, func, args, ret, _loc}) do
    arg_types = Enum.map_join(args, ", ", &format_type/1)
    ret_type = format_type(ret)
    [{func, @kind_function, "(#{arg_types}) → #{ret_type}"}]
  end

  defp extract_definitions(_), do: []

  defp format_type(type) when is_atom(type), do: ":#{type}"
  defp format_type({:list, t}), do: "[#{format_type(t)}]"
  defp format_type({:fn, args, ret}), do: "(#{Enum.map_join(args, ", ", &format_type/1)}) → #{format_type(ret)}"
  defp format_type(other), do: inspect(other)

  # ============================================================================
  # Filtering
  # ============================================================================

  defp extract_partial_word(prefix) do
    # Extract the word being typed (after last whitespace or paren)
    case Regex.run(~r/[a-zA-Z_!?+\-*\/<>=]+$/, prefix) do
      [word] -> word
      nil -> ""
    end
  end

  defp filter_completions(completions, "") do
    completions
  end

  defp filter_completions(completions, partial) do
    partial_lower = String.downcase(partial)

    completions
    |> Enum.filter(fn item ->
      label_lower = String.downcase(item["label"])
      String.starts_with?(label_lower, partial_lower) or
        String.contains?(label_lower, partial_lower)
    end)
    |> Enum.sort_by(fn item ->
      label_lower = String.downcase(item["label"])
      # Prioritize exact prefix matches
      if String.starts_with?(label_lower, partial_lower), do: 0, else: 1
    end)
  end
end
