# Load test support modules
Code.require_file("support/test_helpers.ex", __DIR__)
Code.require_file("support/liquid_gen.ex", __DIR__)

ExUnit.start(exclude: [:live])
