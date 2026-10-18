defmodule Vaisto.Std.NativeMathTest do
  use ExUnit.Case, async: true
  alias Vaisto.{Compilation, Elab, Parser}
  alias Vaisto.Liquid.{Eval, Lint}

  test "the mathematical library compiles without HM and retains Int/Float APIs" do
    huge = Integer.pow(2, 100) + 17

    source =
      File.read!(Path.join(__DIR__, "../../std/Math.va")) <>
        """
        (tuple
          (abs -3) (abs -3.5)
          (clamp 7 0 5) (clamp 7.5 0.0 5.0)
          (sqrt 4) (pow 2 3.0)
          (round #{huge}) (trunc #{huge})
          (round -3.5) (trunc -3.5)
          (e) (deg-to-rad 180) (rad-to-deg (pi)))
        """

    name = :"NativeMath#{System.unique_integer([:positive])}"
    assert {:ok, core} = source |> Parser.parse() |> Elab.module(name: name)
    assert :ok = Lint.check_module(core)
    assert {:ok, ^name, _} = Compilation.compile_liquid(source, name, format_errors: false)

    try do
      value = apply(name, :main, [])
      assert {3, 3.5, 5, 5.0, 2.0, 8.0, ^huge, ^huge, -4, -3, e, radians, degrees} = value

      assert_in_delta e, :math.exp(1), 1.0e-14
      assert_in_delta radians, :math.pi(), 1.0e-14
      assert_in_delta degrees, 180.0, 1.0e-12

      events = [
        {:external, [:math, :sqrt, [4.0]], {:ok, 2.0}},
        {:external, [:math, :pow, [2.0, 3.0]], {:ok, 8.0}},
        {:external, [:math, :exp, [1.0]], {:ok, :math.exp(1)}},
        {:external, [:math, :pi, []], {:ok, :math.pi()}},
        {:external, [:math, :pi, []], {:ok, :math.pi()}},
        {:external, [:math, :pi, []], {:ok, :math.pi()}}
      ]

      assert Eval.run(core, :main, [], handler: {:script, events}) == {:ok, value}
    after
      :code.purge(name)
      :code.delete(name)
    end
  end
end
