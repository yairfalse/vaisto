defmodule Vaisto.DeclaredTypesTest do
  # The acceptance tests of liquid-vaisto-rfc.md P0-1 (D1, D2): a parameter's
  # annotation that names a declared type means that type; and P0-17 (D18): a
  # sum constructor's fields keep their declared types.
  use ExUnit.Case, async: true

  alias Vaisto.{Parser, Runner, TypeChecker, TypeFormatter}

  defp check(source), do: TypeChecker.check(Parser.parse(source))

  defp message(source) do
    assert {:error, errors} = check(source)
    (if is_list(errors), do: hd(errors), else: errors).message
  end

  defp mismatch(source) do
    assert {:error, errors} = check(source)
    e = if is_list(errors), do: hd(errors), else: errors
    assert e.message == "type mismatch"
    {TypeFormatter.format(e.expected), TypeFormatter.format(e.actual)}
  end

  describe "P0-1: annotations name declared types" do
    @point "(deftype Point [x :int y :int])\n(deftype Other [a :int])\n(defn idp [p :Point] :Point p)\n"

    test "a record's name" do
      assert {:ok, _, _} = check(@point <> "(defn main [] :Point (idp (Point 1 2)))")
      assert mismatch(@point <> "(defn main [] :Point (idp (Other 1)))") == {"Point", "Other"}
    end

    @result "(deftype Result (Ok v) (Err e))\n(defn f [r (Result :int :string)] :int 1)\n"

    test "a sum type applied to types" do
      assert {:ok, _, _} = check(@result <> "(defn main [] :int (f (Ok 1)))")
      assert {:error, _} = check(@result <> "(defn main [] :int (f (Ok \"no\")))")
    end

    test "a type declared after the definition that names it" do
      assert {:ok, 4} = Runner.run("(defn f [m Maybe] :int (match m [(Just n) n] [(Nothing) 0]))\n(deftype Maybe (Just :int) (Nothing))\n(defn main [] :int (f (Just 4)))")
    end

    test "a match on an annotated sum must cover it" do
      assert {:error, _} = check("(deftype C (R :int) (G :int))\n(defn f [c C] :int (match c [(R n) n]))")
    end

    test "a declared type takes exactly as many arguments as it has parameters" do
      assert message("(deftype Either (Left :a) (Right :b))\n(defn f [e (Either :int)] :int 1)") == "`Either` takes 2 type arguments, not 1"
      assert message("(deftype Maybe (Just :a) (Nothing))\n(defn f [m (Maybe :int :string)] :int 1)") == "`Maybe` takes 1 type argument, not 2"
      assert message("(defn f [m (Maybe :int)] :int 1)") == "`Maybe` is not a type"
    end

    test "a bare capitalized name after the parameters is the result type" do
      assert {:ok, _, _} = check("(deftype R (Ok :int) (Err :string))\n(defn main [] R (Ok 1))")
      assert message("(deftype R (Ok :int) (Err :string))\n(defn main [] R 5)") =~ "return type mismatch"
    end

    test "a type declared after its use keeps its arguments" do
      box = "(defn get [b Box] :int (match b [(MkBox m) (match m [(Just n) (+ n 1)] [(Nothing) 0])]))\n(deftype Box (MkBox (Maybe :int)))\n(deftype Maybe (Just :a) (Nothing))\n"
      assert {:ok, 5} = Runner.run(box <> "(defn main [] :int (get (MkBox (Just 4))))")
      assert {:error, _} = check(box <> "(defn main [] :int (get (MkBox (Just \"s\"))))")
    end

    test "a record's fields name declared types" do
      assert {:ok, 1} = Runner.run("(deftype Maybe (Just :a) (Nothing))\n(deftype Holder [m (Maybe :int)])\n(defn main [] :int (match (. (Holder (Just 1)) :m) [(Just n) n] [(Nothing) 0]))")
      assert {:ok, 1} = Runner.run("(deftype Point [x :int y :int])\n(deftype Line [a Point b Point])\n(defn main [] :int (. (. (Line (Point 1 2) (Point 3 4)) :a) :x))")
      assert message("(deftype R [x :int next R])") == "the record `R` contains itself in its field `next`"
    end
  end

  describe "P0-17: constructor fields keep their declared types" do
    test "a field declared Int takes only an Int" do
      assert mismatch("(deftype R (Ok :int) (Err :string))\n(defn main [] R (Ok \"s\"))") == {"Int", "String"}
      assert {:ok, _, _} = check("(deftype R (Ok :int) (Err :string))\n(defn main [] R (Ok 1))")
    end

    test "a type variable stays a parameter" do
      maybe = "(deftype Maybe (Just v) (Nothing))\n"
      assert {:ok, _, _} = check(maybe <> "(defn a [] (Maybe :int) (Just 1))\n(defn b [] (Maybe :string) (Just \"s\"))")
    end

    test "a field that names its own type is the value's type, arguments and all" do
      tree = "(deftype Tree (Leaf) (Node (Tree :a) :a (Tree :a)))\n"
      assert {:ok, 6} = Runner.run(tree <> "(defn total (Fn (Tree :int) :int) [(Leaf) 0] [(Node l x r) (+ x (+ (total l) (total r)))])\n(defn main [] :int (total (Node (Node (Leaf) 5 (Leaf)) 1 (Leaf))))")
      # A subtree of a (Tree :int) is a (Tree :int), not a (Tree :string).
      assert {:error, _} = check(tree <> "(defn strs (Fn (Tree :string) :string) [(Leaf) \"\"] [(Node l s r) s])\n(defn sub (Fn (Tree :int) :string) [(Leaf) \"\"] [(Node l _ _) (strs l)])")
      assert {:error, _} = check(tree <> "(defn main [] (Tree :int) (Node (Node (Leaf) \"s\" (Leaf)) 1 (Leaf)))")
    end

    test "a parameter under List, Tuple or Fn in a field takes the type's argument" do
      wrap = "(deftype Wrap (W (List :a)))\n(defn f [w (Wrap :int)] :int (match w [(W xs) (+ 1 (head xs))]))\n"
      assert {:ok, 5} = Runner.run(wrap <> "(defn main [] :int (f (W [4])))")
      assert {:error, _} = check(wrap <> "(defn main [] :int (f (W [\"s\"])))")
    end

    test "types that refer to each other, and a type that changes its own arguments, are errors" do
      assert message("(deftype A (MkA B) (NA))\n(deftype B (MkB A))") =~ "refer to each other"
      assert message("(deftype N (Mk :a (N (List :a))) (E))") =~ "refers to itself with other type arguments"
    end

    test "a field names a type, a type parameter, or a built-in type" do
      assert message("(deftype R (Ok Int))") == "`Int` is not a type"
      # A bare lowercase name is a type parameter, as the elaborator reads it.
      assert {:ok, 1} = Runner.run("(deftype R (Ok int) (Err e))\n(defn main [] :int (match (Ok \"s\") [(Ok v) 1] [(Err e) 2]))")
    end

    test "a nested instance of a recursive type keeps its own recursion" do
      mylist = "(deftype MyList (Nil) (Cons a (MyList a)))\n(defn len [xs (MyList :int)] :int (match xs [(Nil) 0] [(Cons _ r) (+ 1 (len r))]))\n"
      assert {:ok, 3} = Runner.run(mylist <> "(defn total [xss (MyList (MyList :int))] :int (match xss [(Nil) 0] [(Cons xs rest) (+ (len xs) (total rest))]))\n(defn main [] :int (total (Cons (Cons 1 (Nil)) (Cons (Cons 2 (Cons 3 (Nil))) (Nil)))))")
      # The tail of an inner list is a list of Int, so its head is an Int, not a list.
      assert {:error, _} = check(mylist <> "(defn f [xs (MyList (MyList :int))] :int (match xs [(Nil) 0] [(Cons inner _) (match inner [(Nil) 0] [(Cons a rest) (match rest [(Nil) a] [(Cons b _) (match b [(Nil) 100] [(Cons z _) z])])])]))")
    end

    test "a type and a constructor may share a name" do
      assert {:ok, 3} = Runner.run("(deftype Wrapper (Wrapper :int))\n(deftype Box (Box Wrapper))\n(defn main [] :int (match (Box (Wrapper 3)) [(Box (Wrapper n)) n]))")
      assert {:ok, 1} = Runner.run("(deftype Token (Num :int) (Op Op))\n(deftype Op (Plus) (Minus))\n(defn f [o Op] :int (match o [(Plus) 1] [(Minus) 2]))\n(defn main [] :int (f (Plus)))")
    end

    test "a record may contain itself under a list or a function, not directly" do
      assert {:ok, 6} = Runner.run("(deftype Rose [val :int kids (List Rose)])\n(defn total [r Rose] :int (+ (. r :val) (fold + 0 (map total (. r :kids)))))\n(defn main [] :int (total (Rose 1 [(Rose 2 []) (Rose 3 [])])))")
      assert {:ok, _, _} = check("(deftype R [f (Fn R :int)])")
      assert message("(deftype R [x :int next R])") == "the record `R` contains itself in its field `next`"
    end

    test "a Pid names a process, in a field as in an annotation" do
      assert {:ok, _, _} = check("(process counter 0 :inc (+ state 1))\n(deftype Ref (R (Pid counter)))")
    end

    test "a field can name its own type" do
      list = "(deftype IntList (INil) (ICons :int IntList))\n(defn sum [l IntList] :int (match l [(INil) 0] [(ICons h t) (+ h (sum t))]))\n"

      for backend <- [:core, :elixir] do
        assert {:ok, 6} = Runner.run(list <> "(defn main [] :int (sum (ICons 1 (ICons 2 (ICons 3 (INil))))))", backend: backend)
      end
    end
  end
end
