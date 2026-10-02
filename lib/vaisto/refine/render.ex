defmodule Vaisto.Refine.Render do
  @moduledoc """
  Prints Core terms and refinement predicates back in Vaisto syntax, for
  diagnostics (RFC §13.1): a message shows the program as it was written,
  never Core, IR or solver terms.
  """

  alias Vaisto.Parser.Loc

  @ops %{
    add: :+, sub: :-, mul: :*, neg: :-, div: :div, rem: :rem,
    lt: :<, le: :<=, gt: :>, ge: :>=, eq: :==, ne: :!=, not: :not,
    length: :length, empty?: :empty?, head: :head, tail: :tail, concat: :++,
    fadd: :+, fsub: :-, fmul: :*, fdiv: :/, fneg: :-, flt: :<, fle: :<=, fgt: :>, fge: :>=
  }

  @doc "A Core term in Vaisto syntax, with binders under their source names."
  @spec core(term()) :: String.t()
  def core(n) when is_integer(n) or is_float(n), do: to_string(n)
  def core(s) when is_binary(s), do: inspect(s)
  def core(x) when is_atom(x), do: source_name(x)
  def core([:atom, a]), do: ":#{a}"
  def core([:unit]), do: "nil"
  def core([:inj, [:Bool], b]), do: to_string(b)
  def core([:inj, [:List | _], :Nil]), do: "[]"

  def core([:inj, [:List | _], :Cons, _h, _t] = list) do
    case list_elements(list, []) do
      {:ok, elems} -> "[" <> Enum.map_join(elems, " ", &core/1) <> "]"
      :error -> call("cons", tl(tl(tl(list))))
    end
  end

  def core([:inj, _type, ctor]), do: "(#{ctor})"
  def core([:inj, _type, ctor | fields]), do: call(ctor, fields)
  def core([:prim, op | args]), do: call(Map.get(@ops, op, op), args)
  def core([:app, f | args]), do: call(core(f), args)
  def core([:inst, e | _types]), do: core(e)
  def core([:if, c, a, [:inj, [:Bool], false]]), do: call("and", [c, a])
  def core([:if, c, [:inj, [:Bool], true], b]), do: call("or", [c, b])
  def core([:if, c, a, b]), do: call("if", [c, a, b])
  def core([:let, [[:_, _type, e]], body]), do: call("do", [e, body])
  def core([:let, [[x, _type, e]], body]), do: "(let [#{source_name(x)} #{core(e)}] #{core(body)})"
  def core([:fn, params, body]), do: "(fn [#{Enum.map_join(params, " ", fn [x, _] -> source_name(x) end)}] #{core(body)})"
  def core([:select, e, l]), do: "(. #{core(e)} :#{l})"
  def core([:tuple | es]), do: call("tuple", es)
  def core([:match, e | _clauses]), do: "(match #{core(e)} ...)"
  def core([:perform, :crash | _]), do: "(crash)"
  def core(other), do: Vaisto.Liquid.Canonical.render(other)

  @doc """
  A predicate as written, with names replaced by `subst` (a map from name to
  text). This is how a requirement shows the caller's arguments (§13.1).
  """
  @spec surface(term(), %{atom() => String.t()}) :: String.t()
  def surface(n, _subst) when is_integer(n) or is_float(n), do: to_string(n)
  def surface(b, _subst) when is_boolean(b), do: to_string(b)
  def surface(x, subst) when is_atom(x), do: Map.get(subst, x, Atom.to_string(x))
  def surface({:string, s}, _subst), do: inspect(s)
  def surface({:atom, a}, _subst), do: ":#{a}"
  def surface({:call, op, args, %Loc{}}, subst), do: "(" <> Enum.map_join([op | args], " ", &surface_head(&1, subst)) <> ")"
  def surface({:if, c, a, b, %Loc{}}, subst), do: "(if " <> Enum.map_join([c, a, b], " ", &surface(&1, subst)) <> ")"
  def surface(other, _subst), do: inspect(other)

  defp surface_head(op, _subst) when is_atom(op) and op in [:+, :-, :*, :<, :<=, :>, :>=, :==, :!=, :"=>"], do: Atom.to_string(op)
  defp surface_head(arg, subst), do: surface(arg, subst)

  @doc "The name a Core binder had in the source: the adapter writes `x'2` for a renamed `x`."
  @spec source_name(atom()) :: String.t()
  def source_name(x) do
    x |> Atom.to_string() |> String.replace(~r/'\d+$/, "")
  end

  defp call(head, args), do: "(" <> Enum.map_join([head | Enum.map(args, &core/1)], " ", &to_string/1) <> ")"

  defp list_elements([:inj, [:List | _], :Nil], acc), do: {:ok, Enum.reverse(acc)}
  defp list_elements([:inj, [:List | _], :Cons, h, t], acc), do: list_elements(t, [h | acc])
  defp list_elements(_other, _acc), do: :error
end
