defmodule Vaisto.LSP.PositionTest do
  use ExUnit.Case
  alias Vaisto.LSP.Position
  alias Vaisto.Parser.Loc

  describe "from_loc/1" do
    test "converts 1,1 to 0,0" do
      loc = %Loc{line: 1, col: 1}
      pos = Position.from_loc(loc)

      assert pos.line == 0
      assert pos.character == 0
    end

    test "converts 5,10 to 4,9" do
      loc = %Loc{line: 5, col: 10}
      pos = Position.from_loc(loc)

      assert pos.line == 4
      assert pos.character == 9
    end
  end

  describe "to_loc/1" do
    test "converts 0,0 to 1,1" do
      pos = %Position{line: 0, character: 0}
      loc = Position.to_loc(pos)

      assert loc.line == 1
      assert loc.col == 1
    end

    test "converts 4,9 to 5,10" do
      pos = %Position{line: 4, character: 9}
      loc = Position.to_loc(pos)

      assert loc.line == 5
      assert loc.col == 10
    end
  end

  describe "new/2" do
    test "creates position from 0-indexed values" do
      pos = Position.new(3, 7)

      assert pos.line == 3
      assert pos.character == 7
    end
  end

  describe "lsp_to_vaisto/2" do
    test "converts 0-indexed to 1-indexed" do
      assert {1, 1} = Position.lsp_to_vaisto(0, 0)
      assert {5, 10} = Position.lsp_to_vaisto(4, 9)
    end
  end

  describe "vaisto_to_lsp/2" do
    test "converts 1-indexed to 0-indexed" do
      assert {0, 0} = Position.vaisto_to_lsp(1, 1)
      assert {4, 9} = Position.vaisto_to_lsp(5, 10)
    end
  end

  describe "to_lsp_range/2 with Loc" do
    test "creates range from Loc with length" do
      loc = %Loc{line: 3, col: 5}
      range = Position.to_lsp_range(loc, 4)

      assert range == %{
        "start" => %{"line" => 2, "character" => 4},
        "end" => %{"line" => 2, "character" => 8}
      }
    end

    test "creates zero-length range" do
      loc = %Loc{line: 1, col: 1}
      range = Position.to_lsp_range(loc, 0)

      assert range == %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 0}
      }
    end
  end

  describe "to_lsp_range/3 with line/col/length" do
    test "creates range from integers" do
      range = Position.to_lsp_range(3, 5, 4)

      assert range == %{
        "start" => %{"line" => 2, "character" => 4},
        "end" => %{"line" => 2, "character" => 8}
      }
    end
  end

  describe "line_range/3" do
    test "creates range from start to end column" do
      range = Position.line_range(5, 10, 15)

      assert range == %{
        "start" => %{"line" => 4, "character" => 9},
        "end" => %{"line" => 4, "character" => 14}
      }
    end
  end

  describe "to_lsp_position/1 with Loc" do
    test "creates position map from Loc" do
      loc = %Loc{line: 3, col: 7}
      pos = Position.to_lsp_position(loc)

      assert pos == %{"line" => 2, "character" => 6}
    end
  end

  describe "to_lsp_position/2 with line/col" do
    test "creates position map from integers" do
      pos = Position.to_lsp_position(3, 7)

      assert pos == %{"line" => 2, "character" => 6}
    end
  end

  describe "to_map/1" do
    test "converts Position struct to map" do
      pos = %Position{line: 5, character: 10}
      map = Position.to_map(pos)

      assert map == %{"line" => 5, "character" => 10}
    end
  end

  describe "offset/2" do
    test "offsets position by positive delta" do
      pos = %Position{line: 5, character: 10}
      new_pos = Position.offset(pos, 3)

      assert new_pos.line == 5
      assert new_pos.character == 13
    end

    test "offsets position by negative delta" do
      pos = %Position{line: 5, character: 10}
      new_pos = Position.offset(pos, -3)

      assert new_pos.line == 5
      assert new_pos.character == 7
    end
  end

  describe "round-trip conversions" do
    test "Loc -> Position -> Loc preserves values" do
      original = %Loc{line: 42, col: 17}
      roundtrip = original |> Position.from_loc() |> Position.to_loc()

      assert roundtrip.line == original.line
      assert roundtrip.col == original.col
    end

    test "vaisto_to_lsp -> lsp_to_vaisto preserves values" do
      {lsp_line, lsp_col} = Position.vaisto_to_lsp(10, 20)
      {v_line, v_col} = Position.lsp_to_vaisto(lsp_line, lsp_col)

      assert v_line == 10
      assert v_col == 20
    end
  end
end
