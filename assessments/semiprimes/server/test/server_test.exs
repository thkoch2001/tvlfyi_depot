defmodule ServerTest do
  use ExUnit.Case
  doctest Server

  describe "semiprime" do
    test "returns the factors when the number is semiprime" do
      Cache.clear()
      # Semiprimes below 30
      [
        {4, [2, 2]},
        {6, [2, 3]},
        {9, [3, 3]},
        {10, [2, 5]},
        {14, [2, 7]},
        {15, [3, 5]},
        {21, [3, 7]},
        {22, [2, 11]},
        {25, [5, 5]},
        {26, [2, 13]}
      ]
      |> Enum.each(fn {input, expected} ->
        assert Server.semiprime(input) == {:miss, expected}
      end)
    end

    test "returns nothing when the number is a composite number" do
      # Composite numbers below 30
      [1, 2, 3, 5, 7, 8, 11, 12, 13, 16, 17, 18, 19, 20, 23, 24, 27, 28, 29]
      |> Enum.each(fn x ->
        assert Server.semiprime(x) == nil
      end)
    end
  end
end
