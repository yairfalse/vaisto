defmodule Vaisto.NativeRecordChecksTest do
  use ExUnit.Case, async: true
  import Vaisto.TestHelpers, only: [check_type: 1]
  alias Vaisto.{Compilation, Parser, TypeFormatter}

  test "field access retains the parser's located shape" do
    assert {:field_access, :person, :name, %Parser.Loc{}} = Parser.parse("(. person :name)")

    assert {:field_access, {:call, :"get-user", [], _}, :email, _} =
             Parser.parse("(. (get-user) :email)")
  end

  for {source, expected} <- [
        {"(deftype Person [name :string age :int]) (defn name [p :Person] :string (. p :name)) (name (Person \"Ada\" 42))",
         :String},
        {"(deftype Pair [fst :int snd :string]) (defn fst [p :Pair] :int (match p [(Pair f s) f])) (fst (Pair 42 \"x\"))",
         :Int},
        {"(deftype Result (Ok v) (Err e)) (defn unwrap [r (Result :a :a)] :a (match r [(Ok v) v] [(Err e) e])) (unwrap (the (Result :int :int) (Ok 42)))",
         :Int},
        {"(defn get-x [p (Record [x :a] :r)] :a (. p :x)) (get-x \#{:x 42 :extra true})", :Int},
        {"(defn twice-x [p (Record [x :int] :r)] :int (+ (. p :x) (. p :x))) (twice-x \#{:x 21 :extra true})",
         :Int},
        {"(defn sum [p (Record [x :a y :a] :r)] :a where [(Num a)] (+ (. p :x) (. p :y))) (sum \#{:x 20.0 :y 22.0})",
         :Float},
        {"(defn sum-x [p (Record [x :int] :r) q (Record [x :int] :s)] :int (+ (. p :x) (. q :x))) (sum-x \#{:x 20 :a true} \#{:x 22 :b \"s\"})",
         :Int}
      ] do
    test "record contract: #{source}" do
      assert {:ok, unquote(Macro.escape(expected))} = check_type(unquote(source))
    end
  end

  for source <- [
        "(defn get-x [r] (. r :x))",
        "(defn f [r (Record [x :int])] :int (. r :x)) (f \#{:x 1 :y 2})",
        "(defn f [r (Record [x :int] :r)] :int (. r :x)) (f \#{:y 2})",
        "(defn f [r (Record [x :int] :r)] :int (. r :x)) (f \#{:x true})",
        "(deftype Point [x :int]) (defn f [r (Record [x :int] :r)] :int (. r :x)) (f (Point 1))",
        "(deftype Point [x :int]) (. (Point 1) :missing)"
      ] do
    test "record rejection: #{source}" do
      assert {:error, [_ | _]} =
               Compilation.check_liquid(unquote(source), :RecordReject, format_errors: false)
    end
  end

  test "open and closed rows have source-readable type displays" do
    assert TypeFormatter.format([:Record, [:row, [:name, :String], [:rvar, :r]]]) ==
             "{name: String | r}"

    assert TypeFormatter.format([:Record, [:row, [:name, :String], :closed]]) == "{name: String}"
  end
end
