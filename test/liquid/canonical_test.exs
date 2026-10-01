defmodule Vaisto.Liquid.CanonicalTest do
  # Every expectation here is derived from docs/design/liquid-core.md §2.
  use ExUnit.Case, async: true

  alias Vaisto.Liquid.Canonical

  # A fixed corpus of trees covering every kind of leaf and the edge cases
  # §2.2 calls out (zero, negatives, -0.0, empty strings and symbols, nesting).
  @corpus [
    :add,
    :"",
    :"hello world",
    :"42",
    :true,
    0,
    -7,
    123_456_789_012_345_678_901_234_567_890,
    0.0,
    -0.0,
    1.5,
    1.0e300,
    5.0e-324,
    "",
    "hello",
    <<0, 255, 10>>,
    "ünïcödé",
    [],
    [[]],
    [:add, 1, "x"],
    [:module, :Demo, [:"core-version", 0], [:def, :f, [:->, [:Int], [:eff, :closed], :Int], [:fn, [[:x, :Int]], :x]]]
  ]

  describe "§2.2 canonical bytes" do
    test "encode leaves as length-prefixed bytes with display hints" do
      assert Canonical.encode(:add) == "3:add"
      assert Canonical.encode(:"") == "0:"
      assert Canonical.encode(42) == "[1:i]2:42"
      assert Canonical.encode(-7) == "[1:i]2:-7"
      assert Canonical.encode(0) == "[1:i]1:0"
      assert Canonical.encode("hello") == "[1:s]5:hello"
      assert Canonical.encode("") == "[1:s]0:"
    end

    test "encode floats as the 16 lowercase hex digits of their bits" do
      assert Canonical.encode(1.5) == "[1:f]16:3ff8000000000000"
      assert Canonical.encode(0.0) == "[1:f]16:0000000000000000"
      assert Canonical.encode(-0.0) == "[1:f]16:8000000000000000"
    end

    test "encode nodes as their children's encodings between parentheses" do
      assert Canonical.encode([]) == "()"
      assert Canonical.encode([:add, 1, "x"]) == "(3:add[1:i]1:1[1:s]1:x)"
      assert Canonical.encode([[:a], []]) == "((1:a)())"
    end

    test "decode(encode(t)) = t for the corpus" do
      for t <- @corpus do
        assert Canonical.decode(Canonical.encode(t)) == {:ok, t}, "round trip of #{inspect(t)}"
      end
    end

    test "decode(encode(t)) = t for 500 random trees" do
      :rand.seed(:exsss, 2026)

      for _ <- 1..500 do
        t = random_tree(4)
        assert Canonical.decode(Canonical.encode(t)) == {:ok, t}
      end
    end

    test "encode is injective: distinct trees have distinct encodings" do
      confusable = [
        [:"42", 42, "42", 42.0],
        [0.0, -0.0],
        [1, 1.0],
        [[:a], :a, "a"],
        [["ab"], ["a", "b"]],
        [[[]], []],
        [:true, "true"]
      ]

      for group <- confusable do
        encodings = Enum.map(group, &Canonical.encode/1)
        assert length(Enum.uniq(encodings)) == length(group), "collision in #{inspect(group)}"
      end

      :rand.seed(:exsss, 7)
      trees = Enum.uniq(for _ <- 1..500, do: random_tree(3))
      encodings = Enum.map(trees, &Canonical.encode/1)
      assert length(Enum.uniq(encodings)) == length(trees)
    end

    test "decode rejects everything that is not an output of encode" do
      for bad <- [
            "03:abc",
            "3abc",
            "[1:i]2:07",
            "[1:i]2:-0",
            "[1:i]1:x",
            "[1:x]1:a",
            "[1:f]16:3FF8000000000000",
            "[1:f]16:7ff0000000000000",
            "[1:f]16:7ff8000000000000",
            "[1:f]3:1.5",
            "(3:add",
            "3:add)",
            "3:add3:add",
            "9:ab",
            "",
            " 3:add"
          ] do
        assert {:error, _} = Canonical.decode(bad), "accepted #{inspect(bad)}"
      end
    end

    test "decode does not create atoms unless asked to" do
      fresh = "vaisto_canonical_never_an_atom_#{System.unique_integer([:positive])}"
      bytes = "#{byte_size(fresh)}:#{fresh}"

      assert {:error, message} = Canonical.decode(bytes)
      assert message =~ "unknown symbol"
      assert {:ok, atom} = Canonical.decode(bytes, atoms: :create)
      assert Atom.to_string(atom) == fresh
    end

    test "encode rejects values that are not trees" do
      for bad <- [{:a, 1}, %{a: 1}, self(), make_ref(), [1 | 2], <<1::3>>] do
        assert_raise ArgumentError, fn -> Canonical.encode(bad) end
      end
    end
  end

  describe "§2.3 metadata and digests" do
    test "digest is SHA-256 of the canonical bytes, as lowercase hex" do
      # Computed with `shasum -a 256` over the hand-written canonical bytes
      # (3:def1:f[1:i]2:42[1:s]1:s[1:f]16:3ff8000000000000).
      assert Canonical.digest([:def, :f, 42, "s", 1.5]) ==
               "62c02fa9e5c14dc13f865c17a06c30bd48f8597143191443a2ecbaeaa34e4c54"
    end

    test "metadata nodes are excluded from the digest, at any depth" do
      plain = [:module, :Demo, [:"core-version", 0]]

      annotated = [
        :module,
        :Demo,
        [:meta, [:span, "demo.va", 1, 1]],
        [:"core-version", 0, [:meta, [:doc, "the version"]]]
      ]

      # (6:module4:Demo(12:core-version[1:i]1:0)), hashed with shasum.
      assert Canonical.digest(plain) ==
               "e8bb5be1b505b1ef71d7b09f2023e2523ad7e83d30996be8d1bc88bd7991bccf"

      assert Canonical.digest(annotated) == Canonical.digest(plain)
    end

    test "anything other than metadata changes the digest" do
      assert Canonical.digest([:def, :f, 1]) != Canonical.digest([:def, :f, 2])
      assert Canonical.digest([:def, :f, 0.0]) != Canonical.digest([:def, :f, -0.0])
      assert Canonical.digest([:def, [:metadata, 1]]) != Canonical.digest([:def])
    end
  end

  describe "§2.4 the readable form" do
    test "parse(render(t)) = t for the corpus" do
      for t <- @corpus do
        assert Canonical.parse(Canonical.render(t)) == {:ok, t}, "round trip of #{inspect(t)}"
      end
    end

    test "parse(render(t)) = t for 500 random trees" do
      :rand.seed(:exsss, 2027)

      for _ <- 1..500 do
        t = random_tree(4)
        assert Canonical.parse(Canonical.render(t), atoms: :create) == {:ok, t}
      end
    end

    test "symbols that are not plain are written between bars" do
      assert Canonical.render([:add, :"hello world", :"42", :"-1", :"", :"a|b"]) ==
               ~S{(add |hello world| |42| |-1| || |a\|b|)}
    end

    test "strings escape quotes, backslashes and unprintable bytes" do
      assert Canonical.render("say \"hi\"\n") == ~S{"say \"hi\"\x0A;"}
      assert Canonical.render(<<0xFF, ?a>>) == ~S{"\xFF;a"}
      assert Canonical.render("ünï") == ~S{"ünï"}
    end

    test "floats are written in Erlang's short form, integers in decimal" do
      assert Canonical.render([1.5, -0.0, 1.0e300, 42, -7]) == "(1.5 -0.0 1.0e300 42 -7)"
    end

    test "comments and whitespace are ignored" do
      text = """
      ; a module
      (module Demo   ; its name
        (core-version 0))
      """

      assert Canonical.parse(text) == {:ok, [:module, :Demo, [:"core-version", 0]]}
    end

    test "long nodes are indented, one child per line" do
      tree = [:def, :f, [:->, [:Int, :Int], [:eff, :closed], :Int], [:fn, [[:x, :Int], [:y, :Int]], [:prim, :div, :x, [:prim, :add, :y, 1]]]]

      assert Canonical.render(tree) == """
             (def f
               (-> (Int Int) (eff closed) Int)
               (fn ((x Int) (y Int)) (prim div x (prim add y 1))))\
             """
    end

    test "malformed text is an error" do
      for bad <- ["(add", "add)", "\"open", "|open", "01", "-0", "1.2.3", "+1", "(a) (b)", ""] do
        assert {:error, _} = Canonical.parse(bad), "accepted #{inspect(bad)}"
      end
    end
  end

  # A random tree over a small symbol alphabet that exercises every leaf kind.
  defp random_tree(0), do: random_leaf()

  defp random_tree(depth) do
    if :rand.uniform(3) == 1 do
      for _ <- 1..:rand.uniform(4), do: random_tree(depth - 1)
    else
      random_leaf()
    end
  end

  defp random_leaf do
    case :rand.uniform(7) do
      1 -> Enum.random([:a, :add, :"hello world", :"", :"-x", :true, :"ü"])
      2 -> :rand.uniform(2_000_001) - 1_000_001
      3 -> Enum.random([0, 1, -1, 2 ** 70, -(2 ** 70)])
      4 -> Enum.random([0.0, -0.0, 1.5, -2.25, 1.0e300, 5.0e-324, :rand.uniform() * 1000])
      5 -> Enum.random(["", "x", "with space", <<0, 255>>, "quote\"", "ünï"])
      6 -> []
      7 -> :rand.bytes(:rand.uniform(5))
    end
  end
end
