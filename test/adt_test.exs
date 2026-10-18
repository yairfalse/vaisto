defmodule Vaisto.ADTTest do
  use ExUnit.Case
  alias Vaisto.Runner
  alias Vaisto.Error

  # Helper to extract full error text from either string or Error struct
  defp error_text(%Error{} = err), do: Error.to_string(err)
  defp error_text(msg) when is_binary(msg), do: msg

  describe "sum type definition" do
    test "parses sum type syntax" do
      code = "(deftype Result (Ok v) (Error msg))"
      ast = Vaisto.Parser.parse(code)
      assert {:deftype, :Result, {:sum, variants}, _loc} = ast
      assert [{:Ok, [:v]}, {:Error, [:msg]}] = variants
    end

    test "parses sum type with multiple fields" do
      code = "(deftype Either (Left a) (Right b))"
      ast = Vaisto.Parser.parse(code)
      assert {:deftype, :Either, {:sum, [{:Left, [:a]}, {:Right, [:b]}]}, _} = ast
    end

    test "type checks sum type definition" do
      code = "(deftype Option (Some v) (None))"
      ast = Vaisto.Parser.parse(code)
      assert {:ok, [:module, _, _ | items]} = Vaisto.Elab.module(ast)
      assert [:type, :Option, [:v], [:sum, [:Some, [:tvar, :v]], [:None]]] in items
    end
  end

  describe "variant construction" do
    test "constructs Ok variant" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (the (Result :int :string) (Ok 42))
      """
      {:ok, mod} = Runner.compile_and_load(code, :OkCtor)
      assert {:Ok, 42} = Runner.call(mod, :main)
    end

    test "constructs Error variant" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (the (Result :int :string) (Error "boom"))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ErrCtor)
      assert {:Error, "boom"} = Runner.call(mod, :main)
    end

    test "constructs None variant (no fields)" do
      code = """
      (deftype Option (Some v) (None))
      (the (Option :int) (None))
      """
      {:ok, mod} = Runner.compile_and_load(code, :NoneCtor)
      assert {:None} = Runner.call(mod, :main)
    end

    test "constructs Some variant" do
      code = """
      (deftype Option (Some v) (None))
      (Some 100)
      """
      {:ok, mod} = Runner.compile_and_load(code, :SomeCtor)
      assert {:Some, 100} = Runner.call(mod, :main)
    end
  end

  describe "pattern matching on variants" do
    test "matches Ok and extracts value" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (the (Result :int :string) (Ok 42))
        [(Ok v) v]
        [(Error _) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :MatchOk)
      assert 42 = Runner.call(mod, :main)
    end

    test "matches Error and extracts message" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (the (Result :int :string) (Error "failed"))
        [(Ok _) 0]
        [(Error msg) 1])
      """
      {:ok, mod} = Runner.compile_and_load(code, :MatchErr)
      assert 1 = Runner.call(mod, :main)
    end

    test "unwrap function using match" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (defn unwrap [r (Result :a :e) default :a] :a
        (match r
          [(Ok v) v]
          [(Error _) default]))
      (+ (unwrap (the (Result :int :string) (Ok 10)) 0) (unwrap (Error "x") 5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :UnwrapFn)
      assert 15 = Runner.call(mod, :main)
    end

    test "nested variant matching" do
      code = """
      (deftype Option (Some v) (None))
      (defn get-or [opt (Option :a) default :a] :a
        (match opt
          [(Some v) v]
          [(None) default]))
      (get-or (Some 42) 0)
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedMatch)
      assert 42 = Runner.call(mod, :main)
    end
  end

  describe "exhaustiveness checking" do
    test "rejects non-exhaustive match" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (the (Result :int :string) (Ok 1))
        [(Ok v) v])
      """
      result = Runner.compile_and_load(code, :NonExhaustive)
      assert {:error, err} = result
      text = error_text(err)
      assert text =~ "not exhaustive"
      assert text =~ "Error"
    end

    test "accepts exhaustive match" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (the (Result :int :string) (Ok 1))
        [(Ok v) v]
        [(Error _) 0])
      """
      assert {:ok, _} = Runner.compile_and_load(code, :Exhaustive)
    end

    test "accepts catch-all pattern" do
      code = """
      (deftype Result (Ok v) (Error msg))
      (match (the (Result :int :string) (Error "x"))
        [(Ok v) v]
        [_ 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :CatchAll)
      assert 0 = Runner.call(mod, :main)
    end

    test "accepts variable as catch-all" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (match (Green)
        [(Red) 1]
        [other 2])
      """
      {:ok, mod} = Runner.compile_and_load(code, :VarCatchAll)
      assert 2 = Runner.call(mod, :main)
    end

    test "rejects missing multiple variants" do
      code = """
      (deftype Color (Red) (Green) (Blue))
      (match (Red)
        [(Red) 1])
      """
      result = Runner.compile_and_load(code, :MissingMultiple)
      assert {:error, err} = result
      text = error_text(err)
      assert text =~ "not exhaustive"
      # Should mention both missing variants
      assert text =~ "Green" or text =~ "Blue"
    end
  end

  describe "real-world ADT patterns" do
    test "maybe monad pattern" do
      # Simplified version - direct pattern match without higher-order map
      code = """
      (deftype Maybe (Just v) (Nothing))
      (defn double-maybe [m (Maybe :int)] (Maybe :int)
        (match m
          [(Just v) (Just (* v 2))]
          [(Nothing) (Nothing)]))
      (match (double-maybe (Just 21))
        [(Just v) v]
        [(Nothing) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :MaybeMonad)
      assert 42 = Runner.call(mod, :main)
    end

    test "binary tree" do
      code = """
      (deftype Tree (Leaf :int) (Node Tree Tree))
      (defn tree-sum [t :Tree] :int
        (match t
          [(Leaf v) v]
          [(Node l r) (+ (tree-sum l) (tree-sum r))]))
      (tree-sum (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :BinaryTree)
      assert 6 = Runner.call(mod, :main)
    end

    test "list as ADT" do
      code = """
      (deftype Chain (Cons :int Chain) (Nil))
      (defn my-length [lst :Chain] :int
        (match lst
          [(Nil) 0]
          [(Cons _ t) (+ 1 (my-length t))]))
      (my-length (Cons 1 (Cons 2 (Cons 3 (Nil)))))
      """
      {:ok, mod} = Runner.compile_and_load(code, :ListADT)
      assert 3 = Runner.call(mod, :main)
    end
  end

  describe "ADT with product types" do
    test "sum and product types coexist" do
      code = """
      (deftype Point [x :int y :int])
      (deftype Shape (Circle Point :int) (Rectangle Point Point))
      (match (Circle (Point 0 0) 10)
        [(Circle c r) r]
        [(Rectangle _ _) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :SumAndProduct)
      assert 10 = Runner.call(mod, :main)
    end
  end

  describe "parametric polymorphism" do
    test "polymorphic match — Just with int" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (match (Just 42)
        [(Just v) (+ v 1)]
        [(Nothing) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :PolyMatchInt)
      assert 43 = Runner.call(mod, :main)
    end

    test "fresh tvars per call site — Just with int and string in same module" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (defn get-int [] (Maybe :int) (Just 42))
      (defn get-str [] (Maybe :string) (Just "hello"))
      (match (get-int)
        [(Just v) v]
        [(Nothing) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :FreshPerCall)
      assert 42 = Runner.call(mod, :main)
    end

    test "nested ADT — Just wrapping Just" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (match (Just (Just 42))
        [(Just inner) (match inner
          [(Just v) v]
          [(Nothing) 0])]
        [(Nothing) 0])
      """
      {:ok, mod} = Runner.compile_and_load(code, :NestedADT)
      assert 42 = Runner.call(mod, :main)
    end

    test "exhaustiveness still enforced with polymorphic types" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (match (Just 42)
        [(Just v) v])
      """
      result = Runner.compile_and_load(code, :PolyExhaustive)
      assert {:error, err} = result
      text = error_text(err)
      assert text =~ "not exhaustive"
      assert text =~ "Nothing"
    end

    test "two-param ADT — Result with Ok and Err" do
      code = """
      (deftype Result (Ok v) (Err e))
      (defn unwrap-or [r (Result :a :e) default :a] :a
        (match r
          [(Ok v) v]
          [(Err _) default]))
      (+ (unwrap-or (the (Result :int :string) (Ok 10)) 0) (unwrap-or (Err "fail") 5))
      """
      {:ok, mod} = Runner.compile_and_load(code, :TwoParamADT)
      assert 15 = Runner.call(mod, :main)
    end

    test "branch type mismatch is detected" do
      code = """
      (deftype Maybe (Just v) (Nothing))
      (defn bad [m (Maybe :int)] :int
        (match m
          [(Just v) "hello"]
          [(Nothing) 42]))
      """
      result = Runner.compile_and_load(code, :BranchMismatch)
      assert {:error, err} = result
      text = error_text(err)
      assert text =~ "branch" or text =~ "mismatch" or text =~ "cannot"
    end
  end
end
