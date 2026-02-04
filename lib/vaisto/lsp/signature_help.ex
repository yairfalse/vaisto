defmodule Vaisto.LSP.SignatureHelp do
  @moduledoc """
  LSP signature help provider for Vaisto.

  Shows function signatures and parameter info while typing.
  Uses parenthesis counting to find the enclosing function call
  and determine which parameter is currently being typed.
  """

  alias Vaisto.Parser
  alias Vaisto.LSP.ASTAnalyzer
  alias Vaisto.TypeFormatter

  # ============================================================================
  # Built-in Function Signatures
  # ============================================================================

  @signatures %{
    # Arithmetic
    "+" => %{
      label: "(+ a b) → num",
      doc: "Adds two numbers. Works with int or float.",
      params: [
        %{label: "a", doc: "First operand (int or float)"},
        %{label: "b", doc: "Second operand (int or float)"}
      ]
    },
    "-" => %{
      label: "(- a b) → num",
      doc: "Subtracts b from a.",
      params: [
        %{label: "a", doc: "Minuend (int or float)"},
        %{label: "b", doc: "Subtrahend (int or float)"}
      ]
    },
    "*" => %{
      label: "(* a b) → num",
      doc: "Multiplies two numbers.",
      params: [
        %{label: "a", doc: "First factor (int or float)"},
        %{label: "b", doc: "Second factor (int or float)"}
      ]
    },
    "/" => %{
      label: "(/ a b) → float",
      doc: "Divides a by b. Always returns float.",
      params: [
        %{label: "a", doc: "Dividend (int or float)"},
        %{label: "b", doc: "Divisor (int or float)"}
      ]
    },

    # Comparison
    "<" => %{
      label: "(< a b) → bool",
      doc: "Returns true if a is less than b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },
    ">" => %{
      label: "(> a b) → bool",
      doc: "Returns true if a is greater than b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },
    "<=" => %{
      label: "(<= a b) → bool",
      doc: "Returns true if a is less than or equal to b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },
    ">=" => %{
      label: "(>= a b) → bool",
      doc: "Returns true if a is greater than or equal to b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },
    "==" => %{
      label: "(== a b) → bool",
      doc: "Returns true if a equals b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },
    "!=" => %{
      label: "(!= a b) → bool",
      doc: "Returns true if a does not equal b.",
      params: [
        %{label: "a", doc: "Left operand"},
        %{label: "b", doc: "Right operand"}
      ]
    },

    # Boolean
    "and" => %{
      label: "(and a b) → bool",
      doc: "Logical AND. Returns true if both a and b are true.",
      params: [
        %{label: "a", doc: "First boolean"},
        %{label: "b", doc: "Second boolean"}
      ]
    },
    "or" => %{
      label: "(or a b) → bool",
      doc: "Logical OR. Returns true if either a or b is true.",
      params: [
        %{label: "a", doc: "First boolean"},
        %{label: "b", doc: "Second boolean"}
      ]
    },
    "not" => %{
      label: "(not a) → bool",
      doc: "Logical NOT. Returns true if a is false.",
      params: [
        %{label: "a", doc: "Boolean to negate"}
      ]
    },

    # List operations
    "list" => %{
      label: "(list elem...) → [elem]",
      doc: "Creates a list from the given elements. All elements must have the same type.",
      params: [
        %{label: "elem...", doc: "Elements to include in the list"}
      ]
    },
    "head" => %{
      label: "(head xs) → elem",
      doc: "Returns the first element of a list. Throws if list is empty.",
      params: [
        %{label: "xs", doc: "A non-empty list"}
      ]
    },
    "tail" => %{
      label: "(tail xs) → [elem]",
      doc: "Returns all elements except the first. Throws if list is empty.",
      params: [
        %{label: "xs", doc: "A non-empty list"}
      ]
    },
    "cons" => %{
      label: "(cons x xs) → [x]",
      doc: "Prepends element x to list xs.",
      params: [
        %{label: "x", doc: "Element to prepend"},
        %{label: "xs", doc: "List to prepend to"}
      ]
    },
    "length" => %{
      label: "(length xs) → int",
      doc: "Returns the number of elements in the list.",
      params: [
        %{label: "xs", doc: "A list"}
      ]
    },
    "map" => %{
      label: "(map fn xs) → [b]",
      doc: "Applies fn to each element of xs, returning a new list.",
      params: [
        %{label: "fn", doc: "Function (a → b) to apply"},
        %{label: "xs", doc: "List of elements"}
      ]
    },
    "filter" => %{
      label: "(filter pred xs) → [a]",
      doc: "Returns elements of xs for which pred returns true.",
      params: [
        %{label: "pred", doc: "Predicate function (a → bool)"},
        %{label: "xs", doc: "List to filter"}
      ]
    },
    "fold" => %{
      label: "(fold fn init xs) → b",
      doc: "Reduces xs to a single value using fn and starting value init.",
      params: [
        %{label: "fn", doc: "Reducer function (acc, elem → acc)"},
        %{label: "init", doc: "Initial accumulator value"},
        %{label: "xs", doc: "List to fold over"}
      ]
    },

    # String
    "str" => %{
      label: "(str arg...) → string",
      doc: "Converts all arguments to strings and concatenates them.",
      params: [
        %{label: "arg...", doc: "Values to convert and concatenate"}
      ]
    },

    # Process
    "spawn" => %{
      label: "(spawn process init) → pid",
      doc: "Spawns a new process with the given initial state.",
      params: [
        %{label: "process", doc: "Process definition name"},
        %{label: "init", doc: "Initial state value"}
      ]
    },
    "!" => %{
      label: "(! pid msg) → msg",
      doc: "Sends msg to pid. Type-checked: msg must be valid for the process.",
      params: [
        %{label: "pid", doc: "Process ID to send to"},
        %{label: "msg", doc: "Message (must match process handlers)"}
      ]
    },
    "!!" => %{
      label: "(!! pid msg) → msg",
      doc: "Sends msg to pid. Unchecked: any message allowed.",
      params: [
        %{label: "pid", doc: "Process ID to send to"},
        %{label: "msg", doc: "Message (any value)"}
      ]
    },

    # Control flow (special forms)
    "if" => %{
      label: "(if cond then else) → result",
      doc: "Conditional expression. Returns then if cond is true, else otherwise.",
      params: [
        %{label: "cond", doc: "Condition (must be bool)"},
        %{label: "then", doc: "Value if true"},
        %{label: "else", doc: "Value if false"}
      ]
    },
    "let" => %{
      label: "(let [name value] body) → result",
      doc: "Binds name to value within body.",
      params: [
        %{label: "[name value]", doc: "Binding: name = value"},
        %{label: "body", doc: "Expression using the binding"}
      ]
    },
    "match" => %{
      label: "(match expr [pat1 res1] [pat2 res2] ...) → result",
      doc: "Pattern matching. Matches expr against patterns and returns corresponding result.",
      params: [
        %{label: "expr", doc: "Expression to match"},
        %{label: "[pattern result]...", doc: "Pattern-result pairs"}
      ]
    },
    "cond" => %{
      label: "(cond [test1 res1] [test2 res2] [:else default]) → result",
      doc: "Multi-way conditional. Evaluates tests in order, returns first matching result.",
      params: [
        %{label: "[test result]...", doc: "Condition-result pairs"},
        %{label: "[:else default]", doc: "Optional default case"}
      ]
    },
    "do" => %{
      label: "(do expr1 expr2 ... result) → result",
      doc: "Evaluates expressions in sequence, returns the last one.",
      params: [
        %{label: "expr...", doc: "Expressions to evaluate"},
        %{label: "result", doc: "Final expression (returned)"}
      ]
    },
    "fn" => %{
      label: "(fn [params...] body) → function",
      doc: "Creates an anonymous function.",
      params: [
        %{label: "[params...]", doc: "Parameter names"},
        %{label: "body", doc: "Function body"}
      ]
    },

    # Tuple
    "tuple" => %{
      label: "(tuple elem...) → tuple",
      doc: "Creates a tuple from the given elements.",
      params: [
        %{label: "elem...", doc: "Elements for the tuple"}
      ]
    }
  }

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Get signature help for the current cursor position.
  Returns an LSP SignatureHelp object or nil if no signature is available.
  """
  def get_signature_help(text, line, col) do
    # Get text up to cursor position
    lines = String.split(text, "\n")
    prefix = get_text_up_to(lines, line, col)

    # Find the enclosing function call
    case find_enclosing_call(prefix) do
      {:ok, func_name, arg_index} ->
        # Look up signature from builtins or user definitions
        case get_signature(func_name, text) do
          nil -> nil
          sig -> build_signature_help(sig, arg_index)
        end

      :not_found ->
        nil
    end
  end

  # ============================================================================
  # Core Algorithm: Find Enclosing Call
  # ============================================================================

  @doc """
  Walk backwards through text to find the enclosing function call.
  Returns {:ok, function_name, argument_index} or :not_found.

  For Lisp-like syntax, we count parentheses to find the unclosed opening paren,
  then extract the function name that follows it.

  We walk backwards through the text, tracking:
  - depth: nesting level (close paren increases, open paren decreases)
  - arg_count: number of top-level arguments seen (spaces at depth 0)
  - acc: characters accumulated since last significant position

  When we find an unclosed open paren (depth becomes 0), we look at acc
  to find the function name that immediately follows it.
  """
  def find_enclosing_call(text) do
    chars = String.graphemes(text) |> Enum.reverse()
    find_enclosing_call(chars, 0, 0, [])
  end

  # Found unclosed opening paren - extract function name from accumulated chars
  # acc contains chars after the "(" in reverse order, so we need to process it
  defp find_enclosing_call(["(" | _rest], 0, arg_count, acc) do
    # acc is in order (first char of func name first) because we prepend to it
    # Skip leading whitespace and extract function name
    func_name = acc
                |> Enum.drop_while(&(&1 in [" ", "\t", "\n", "\r"]))
                |> Enum.take_while(&(&1 not in [" ", "\t", "\n", "\r", "(", ")", "[", "]"]))
                |> Enum.join()

    if func_name != "" do
      {:ok, func_name, arg_count}
    else
      :not_found
    end
  end

  # Close paren increases depth (we're inside a nested expression)
  defp find_enclosing_call([")" | rest], depth, arg_count, acc) do
    find_enclosing_call(rest, depth + 1, arg_count, [")" | acc])
  end

  # Open paren at depth > 0 decreases depth (closing a nested expression)
  defp find_enclosing_call(["(" | rest], depth, arg_count, acc) when depth > 0 do
    find_enclosing_call(rest, depth - 1, arg_count, ["(" | acc])
  end

  # Space at depth 0 might be an argument separator
  defp find_enclosing_call([" " | rest], 0, arg_count, acc) do
    # Check if we just finished a token (not just leading whitespace)
    # A token ends when we see space after non-space chars
    has_token = Enum.any?(acc, &(&1 not in [" ", "\t", "\n", "\r"]))

    # Count argument if we just finished a token
    new_count = if has_token do
      # But don't count if acc is just the function name (first token)
      # We'll count it as an arg only if there's already content
      arg_count + 1
    else
      arg_count
    end

    find_enclosing_call(rest, 0, new_count, [" " | acc])
  end

  # Other characters at any depth
  defp find_enclosing_call([char | rest], depth, arg_count, acc) do
    find_enclosing_call(rest, depth, arg_count, [char | acc])
  end

  # End of text without finding unclosed paren
  defp find_enclosing_call([], _depth, _arg_count, _acc), do: :not_found

  # ============================================================================
  # Text Extraction
  # ============================================================================

  defp get_text_up_to(lines, line, col) do
    lines
    |> Enum.take(line)
    |> Enum.with_index(1)
    |> Enum.map(fn {text, line_num} ->
      if line_num == line do
        String.slice(text, 0, col - 1)
      else
        text
      end
    end)
    |> Enum.join("\n")
  end

  # ============================================================================
  # Signature Lookup
  # ============================================================================

  defp get_signature(func_name, text) do
    # First check built-ins
    case Map.get(@signatures, func_name) do
      nil ->
        # Try to find user-defined function
        get_user_signature(func_name, text)
      sig ->
        sig
    end
  end

  defp get_user_signature(func_name, text) do
    try do
      ast = Parser.parse(text)
      func_atom = String.to_atom(func_name)

      # Use ASTAnalyzer to find the function definition
      case ASTAnalyzer.find_definition_at(ast, func_atom) do
        {:ok, _loc} ->
          # Found it - now extract params from the definition
          defs = ASTAnalyzer.extract_definitions(ast)
          case Enum.find(defs, &(&1.name == func_atom && &1.kind == :function)) do
            %{arity: arity} ->
              # Get params from AST directly for signature building
              find_function_params(ast, func_atom)
            _ -> nil
          end
        :not_found -> nil
      end
    rescue
      _ -> nil
    end
  end

  defp find_function_params(ast, func_name) when is_list(ast) do
    Enum.find_value(ast, &find_function_params(&1, func_name))
  end

  defp find_function_params({:defn, name, params, _body, _loc}, name) do
    build_user_signature(to_string(name), params, nil)
  end

  defp find_function_params({:defn, name, params, _body, ret_type, _loc}, name) do
    build_user_signature(to_string(name), params, ret_type)
  end

  defp find_function_params(_, _), do: nil

  defp build_user_signature(name, params, ret_type) do
    param_strs = Enum.map(params, fn
      {param_name, type} -> "#{param_name} :#{type}"
      {:var, param_name, _} -> to_string(param_name)
      param_name when is_atom(param_name) -> to_string(param_name)
      other -> inspect(other)
    end)

    ret_str = if ret_type, do: " → :#{ret_type}", else: ""
    label = "(#{name} #{Enum.join(param_strs, " ")})#{ret_str}"

    param_docs = Enum.map(params, fn
      {param_name, type} ->
        %{label: to_string(param_name), doc: "Type: :#{type}"}
      {:var, param_name, type} ->
        %{label: to_string(param_name), doc: "Type: #{format_type(type)}"}
      param_name when is_atom(param_name) ->
        %{label: to_string(param_name), doc: "Parameter"}
      _ ->
        %{label: "?", doc: "Parameter"}
    end)

    %{
      label: label,
      doc: "User-defined function",
      params: param_docs
    }
  end

  defp format_type(type), do: TypeFormatter.format(type)

  # ============================================================================
  # Response Building
  # ============================================================================

  defp build_signature_help(sig, active_param) do
    # Build parameter labels with positions for highlighting
    params_with_positions = calculate_param_positions(sig.label, sig.params)

    %{
      "signatures" => [
        %{
          "label" => sig.label,
          "documentation" => %{
            "kind" => "markdown",
            "value" => sig.doc
          },
          "parameters" => Enum.map(params_with_positions, fn {param, start_pos, end_pos} ->
            %{
              "label" => [start_pos, end_pos],
              "documentation" => %{
                "kind" => "markdown",
                "value" => param.doc
              }
            }
          end)
        }
      ],
      "activeSignature" => 0,
      "activeParameter" => min(active_param, length(sig.params) - 1)
    }
  end

  # Calculate character positions for each parameter in the label
  defp calculate_param_positions(label, params) do
    # Find positions after the opening paren and function name
    case Regex.run(~r/^\([^\s]+\s+/, label) do
      [prefix] ->
        start = String.length(prefix)
        calculate_positions(params, label, start, [])
      nil ->
        # Fallback: just use the label text
        Enum.map(params, fn param -> {param, 0, 0} end)
    end
  end

  defp calculate_positions([], _label, _pos, acc), do: Enum.reverse(acc)
  defp calculate_positions([param | rest], label, pos, acc) do
    param_label = param.label
    case :binary.match(label, param_label, [{:scope, {pos, byte_size(label) - pos}}]) do
      {start, len} ->
        calculate_positions(rest, label, start + len, [{param, start, start + len} | acc])
      :nomatch ->
        calculate_positions(rest, label, pos, [{param, pos, pos} | acc])
    end
  end
end
