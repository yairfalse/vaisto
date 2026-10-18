defmodule Vaisto.UnsafeSendTest do
  use ExUnit.Case, async: true

  alias Vaisto.Runner

  describe "unsafe send (!!)" do
    test "!! accepts any message to typed PID" do
      # Even invalid messages should pass with !!
      code = """
      (process counter 0
        :inc (+ state 1))

      (defn main [] :unit
        (!! (spawn counter 0) :totally-invalid-message))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeSendAnyMsg)
    end

    test "!! accepts valid message to typed PID" do
      code = """
      (process counter 0
        :inc (+ state 1))

      (defn main [] :unit
        (!! (spawn counter 0) :inc))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeSendValidMsg)
    end

    test "!! rejects non-PID first argument" do
      code = """
      (defn main [] :unit
        (!! 42 :message))
      """
      result = Runner.compile_and_load(code, :UnsafeSendNonPid)
      assert {:error, %Vaisto.Error{message: msg}} = result
      assert msg =~ "non-pid" or msg =~ "PID"
    end

    test "!! accepts a PID parameter with an explicit protocol" do
      code = """
      (process echo 0
        :ping state)

      (defn send_msg [pid (Pid :Process.echo.Message)] :unit
        (!! pid :ping))

      (defn main [] :unit
        (send_msg (spawn echo 0)))
      """
      assert {:ok, _} = Runner.compile_and_load(code, :UnsafeSendUntypedPid)
    end

    test "! rejects invalid message (for comparison)" do
      # Verify that ! still validates - this is the safe version
      code = """
      (process counter 0
        :inc (+ state 1))

      (defn main [] :unit
        (! (spawn counter 0) :invalid))
      """
      result = Runner.compile_and_load(code, :SafeSendInvalid)
      assert {:error, %Vaisto.Error{message: message}} = result
      assert message =~ "does not accept"
    end
  end
end
