defmodule MathTest do
  use ExUnit.Case
  doctest Math

  describe "factor" do
    test "returns the prime factors for an input" do
      [
        {15, [3, 5]},
        {12, [2, 2, 3]},
        {9, [3, 3]},
        {21, [3, 7]}
      ]
      |> Enum.map(fn {input, expected} ->
        assert Math.factor(input) == expected
      end)
    end

    test "handles large numbers" do
      assert Math.factor(104_023) == [17, 29, 211]
    end

    test "returns an empty list for 1" do
      assert Math.factor(1) == []
    end

    test "returns the prime number itself when the input is prime" do
      assert Math.factor(7) == [7]
    end
  end
end
