defmodule Vaisto.Liquid.Canonical do
  @moduledoc """
  Canonical trees, the one format of every Liquid Core artifact.

  Implements `docs/design/liquid-core.md` §2. A tree is a leaf (an atom for a
  symbol, an integer, a float or a binary for a string) or a proper list of
  trees. Each tree has two renderings that decode to it:

    * `encode/1` gives the canonical bytes, which `digest/1` hashes;
    * `render/1` gives the indented readable form, which `parse/2` reads.

  Decoding creates no atoms unless asked to (`atoms: :create`), because atoms
  are never garbage collected and decoded input may be untrusted.
  """

  import Bitwise

  @type tree :: atom() | integer() | float() | binary() | [tree()]

  # --- §2.2 Canonical bytes ---------------------------------------------------

  @doc "The canonical bytes of a tree."
  @spec encode(tree()) :: binary()
  def encode(tree), do: tree |> enc() |> IO.iodata_to_binary()

  defp enc(t) when is_atom(t), do: prefixed(Atom.to_string(t))
  defp enc(t) when is_integer(t), do: ["[1:i]", prefixed(Integer.to_string(t))]
  defp enc(t) when is_float(t), do: ["[1:f]16:", float_hex(t)]
  defp enc(t) when is_binary(t), do: ["[1:s]", prefixed(t)]
  defp enc(t) when is_list(t), do: ["(", enc_children(t), ")"]
  defp enc(t), do: raise(ArgumentError, "not a tree: #{inspect(t)}")

  defp enc_children([]), do: []
  defp enc_children([h | t]), do: [enc(h) | enc_children(t)]
  defp enc_children(t), do: raise(ArgumentError, "not a tree: improper list ending in #{inspect(t)}")

  defp prefixed(bytes), do: [Integer.to_string(byte_size(bytes)), ":", bytes]

  defp float_hex(f) do
    <<bits::64>> = <<f::float-64>>
    bits |> Integer.to_string(16) |> String.downcase() |> String.pad_leading(16, "0")
  end

  @doc """
  Decode canonical bytes. Accepts exactly the outputs of `encode/1`.

  Options: `atoms: :existing` (the default) reports a symbol that is not
  already an atom as an error; `atoms: :create` creates it.
  """
  @spec decode(binary(), keyword()) :: {:ok, tree()} | {:error, String.t()}
  def decode(bytes, opts \\ []) when is_binary(bytes) do
    atoms = Keyword.get(opts, :atoms, :existing)

    case dec(bytes, atoms) do
      {tree, ""} -> {:ok, tree}
      {_tree, rest} -> {:error, "trailing bytes at offset #{byte_size(bytes) - byte_size(rest)}"}
    end
  catch
    {:canonical, message} -> {:error, message}
  end

  defp dec("(" <> rest, atoms), do: dec_children(rest, atoms, [])
  defp dec("[1:i]" <> rest, _atoms) do
    {digits, rest} = take_prefixed(rest)
    {parse_canonical_integer(digits), rest}
  end
  defp dec("[1:s]" <> rest, _atoms), do: take_prefixed(rest)
  defp dec("[1:f]16:" <> <<hex::binary-16, rest::binary>>, _atoms), do: {parse_float_hex(hex), rest}
  defp dec("[" <> _, _atoms), do: fail("unknown or malformed display hint")
  defp dec(<<d, _::binary>> = bytes, atoms) when d in ?0..?9 do
    {name, rest} = take_prefixed(bytes)
    {to_atom(name, atoms), rest}
  end
  defp dec("", _atoms), do: fail("unexpected end of input")
  defp dec(<<c, _::binary>>, _atoms), do: fail("unexpected byte #{inspect(<<c>>)}")

  defp dec_children(")" <> rest, _atoms, acc), do: {Enum.reverse(acc), rest}
  defp dec_children("", _atoms, _acc), do: fail("unbalanced parenthesis")
  defp dec_children(bytes, atoms, acc) do
    {child, rest} = dec(bytes, atoms)
    dec_children(rest, atoms, [child | acc])
  end

  defp take_prefixed(bytes) do
    case :binary.split(bytes, ":") do
      [len, rest] ->
        n = parse_length(len)
        if byte_size(rest) < n, do: fail("length #{n} runs past the end of input")
        <<taken::binary-size(^n), rest::binary>> = rest
        {taken, rest}

      [_] ->
        fail("missing ':' after a length")
    end
  end

  defp parse_length("0"), do: 0
  defp parse_length(<<d, _::binary>> = len) when d in ?1..?9 do
    case Integer.parse(len) do
      {n, ""} -> n
      _ -> fail("malformed length #{inspect(len)}")
    end
  end
  defp parse_length(len), do: fail("malformed length #{inspect(len)}")

  defp parse_canonical_integer(digits) do
    with true <- digits =~ ~r/\A-?(0|[1-9][0-9]*)\z/,
         false <- digits == "-0" do
      String.to_integer(digits)
    else
      _ -> fail("non-canonical integer #{inspect(digits)}")
    end
  end

  defp parse_float_hex(hex) do
    unless hex =~ ~r/\A[0-9a-f]{16}\z/, do: fail("malformed float #{inspect(hex)}")
    bits = String.to_integer(hex, 16)
    if (bits >>> 52 &&& 0x7FF) == 0x7FF, do: fail("float is not finite")
    <<f::float-64>> = <<bits::64>>
    f
  end

  defp to_atom(name, :create) do
    String.to_atom(name)
  rescue
    ArgumentError -> fail("symbol #{inspect(name)} cannot be an atom")
  end

  defp to_atom(name, :existing) do
    String.to_existing_atom(name)
  rescue
    ArgumentError -> fail("unknown symbol #{inspect(name)}")
  end

  defp fail(message), do: throw({:canonical, message})

  # --- §2.3 Metadata and digests ------------------------------------------------

  @doc "Remove every `(meta ...)` node that is a child of another node."
  @spec strip(tree()) :: tree()
  def strip(tree) when is_list(tree), do: for(child <- tree, not meta?(child), do: strip(child))
  def strip(tree), do: tree

  defp meta?([:meta | _]), do: true
  defp meta?(_), do: false

  @doc "SHA-256 of the canonical bytes of the tree without metadata, as lowercase hex."
  @spec digest(tree()) :: String.t()
  def digest(tree) do
    :crypto.hash(:sha256, encode(strip(tree))) |> Base.encode16(case: :lower)
  end

  # --- §2.4 The readable form -------------------------------------------------

  @width 80

  @doc "The indented readable form of a tree."
  @spec render(tree()) :: String.t()
  def render(tree), do: tree |> render_at(0) |> IO.iodata_to_binary()

  defp render_at(tree, indent) when is_list(tree) do
    line = flat(tree)

    if tree == [] or indent + IO.iodata_length(line) <= @width do
      line
    else
      # `(def name` stays on one line; the remaining children go one per line.
      {opening, rest} =
        case tree do
          [head, name | rest] when is_atom(head) and not is_list(name) ->
            {[render_leaf(head), " ", render_leaf(name)], rest}

          [first | rest] ->
            {render_at(first, indent + 1), rest}
        end

      pad = ["\n", String.duplicate(" ", indent + 2)]
      ["(", opening, Enum.map(rest, &[pad, render_at(&1, indent + 2)]), ")"]
    end
  end

  defp render_at(leaf, _indent), do: render_leaf(leaf)

  defp flat(tree) when is_list(tree), do: ["(", tree |> Enum.map(&flat/1) |> Enum.intersperse(" "), ")"]
  defp flat(leaf), do: render_leaf(leaf)

  defp render_leaf(t) when is_integer(t), do: Integer.to_string(t)
  defp render_leaf(t) when is_float(t), do: :erlang.float_to_binary(t, [:short])
  defp render_leaf(t) when is_binary(t), do: ["\"", escape(t, ?"), "\""]

  defp render_leaf(t) when is_atom(t) do
    name = Atom.to_string(t)
    if plain?(name), do: name, else: ["|", escape(name, ?|), "|"]
  end

  defp render_leaf(t), do: raise(ArgumentError, "not a tree: #{inspect(t)}")

  @plain_punctuation ~c"!$%&*+-./:<=>?@^_~#"

  defp plain?(""), do: false
  defp plain?(name) do
    looks_numeric?(name) == false and
      name |> String.to_charlist() |> Enum.all?(&plain_char?/1)
  end

  defp plain_char?(c), do: c in ?a..?z or c in ?A..?Z or c in ?0..?9 or c in @plain_punctuation

  defp looks_numeric?(<<d, _::binary>>) when d in ?0..?9, do: true
  defp looks_numeric?(<<s, d, _::binary>>) when s in [?-, ?+] and d in ?0..?9, do: true
  defp looks_numeric?(_), do: false

  # Valid UTF-8 keeps its printable characters; everything else is \xHH;.
  defp escape(bytes, quote) do
    if String.valid?(bytes) do
      for <<c::utf8 <- bytes>>, do: escape_char(c, quote)
    else
      for <<b <- bytes>>, do: if(b < 0x80, do: escape_char(b, quote), else: hex_escape(b))
    end
  end

  defp escape_char(c, quote) when c == quote or c == ?\\, do: [?\\, c]
  defp escape_char(c, _quote) when c < 0x20 or c == 0x7F, do: hex_escape(c)
  defp escape_char(c, _quote), do: <<c::utf8>>

  defp hex_escape(b), do: ["\\x", b |> Integer.to_string(16) |> String.pad_leading(2, "0"), ";"]

  @doc """
  Parse the readable form. Options as for `decode/2`.
  """
  @spec parse(String.t(), keyword()) :: {:ok, tree()} | {:error, String.t()}
  def parse(text, opts \\ []) when is_binary(text) do
    atoms = Keyword.get(opts, :atoms, :existing)

    case read(skip(text), atoms) do
      {tree, rest} ->
        case skip(rest) do
          "" -> {:ok, tree}
          more -> {:error, "unexpected text after the tree: #{inspect(String.slice(more, 0, 20))}"}
        end
    end
  catch
    {:canonical, message} -> {:error, message}
  end

  @doc "Like `parse/2`, raising on error."
  @spec parse!(String.t(), keyword()) :: tree()
  def parse!(text, opts \\ []) do
    case parse(text, opts) do
      {:ok, tree} -> tree
      {:error, message} -> raise ArgumentError, "readable tree: " <> message
    end
  end

  defp skip(<<c, rest::binary>>) when c in ~c" \t\r\n", do: skip(rest)
  defp skip(";" <> rest) do
    case :binary.split(rest, "\n") do
      [_comment, after_comment] -> skip(after_comment)
      [_comment] -> ""
    end
  end
  defp skip(text), do: text

  defp read("(" <> rest, atoms), do: read_children(skip(rest), atoms, [])
  defp read(")" <> _, _atoms), do: fail("unexpected ')'")
  defp read("\"" <> rest, _atoms), do: read_quoted(rest, ?", [])
  defp read("|" <> rest, atoms) do
    {name, rest} = read_quoted(rest, ?|, [])
    {to_atom(name, atoms), rest}
  end
  defp read("", _atoms), do: fail("unexpected end of input")
  defp read(text, atoms) do
    {token, rest} = take_token(text, [])
    {classify(token, atoms), rest}
  end

  defp read_children(")" <> rest, _atoms, acc), do: {Enum.reverse(acc), rest}
  defp read_children("", _atoms, _acc), do: fail("unbalanced parenthesis")
  defp read_children(text, atoms, acc) do
    {child, rest} = read(text, atoms)
    read_children(skip(rest), atoms, [child | acc])
  end

  defp read_quoted(<<q, rest::binary>>, q, acc), do: {acc |> Enum.reverse() |> IO.iodata_to_binary(), rest}
  defp read_quoted("\\x" <> <<hex::binary-2, ";", rest::binary>>, q, acc) do
    case Integer.parse(hex, 16) do
      {b, ""} -> read_quoted(rest, q, [<<b>> | acc])
      _ -> fail("malformed escape \\x#{hex};")
    end
  end
  defp read_quoted(<<?\\, c, rest::binary>>, q, acc) when c == q or c == ?\\, do: read_quoted(rest, q, [<<c>> | acc])
  defp read_quoted(<<?\\, _::binary>>, _q, _acc), do: fail("malformed escape")
  defp read_quoted(<<c, rest::binary>>, q, acc), do: read_quoted(rest, q, [<<c>> | acc])
  defp read_quoted("", _q, _acc), do: fail("unterminated string or symbol")

  defp take_token(<<c, _::binary>> = rest, acc) when c in ~c" \t\r\n()\";|",
    do: {acc |> Enum.reverse() |> IO.iodata_to_binary(), rest}
  defp take_token(<<c, rest::binary>>, acc), do: take_token(rest, [<<c>> | acc])
  defp take_token("", acc), do: {acc |> Enum.reverse() |> IO.iodata_to_binary(), ""}

  defp classify(token, atoms) do
    cond do
      not looks_numeric?(token) ->
        if plain?(token), do: to_atom(token, atoms), else: fail("symbol #{inspect(token)} must be written between bars")

      token =~ ~r/\A-?(0|[1-9][0-9]*)\z/ and token != "-0" ->
        String.to_integer(token)

      String.contains?(token, ".") ->
        try do
          :erlang.binary_to_float(token)
        rescue
          ArgumentError -> fail("malformed float #{inspect(token)}")
        end

      true ->
        fail("malformed number #{inspect(token)}")
    end
  end
end
