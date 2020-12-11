defmodule ExtrasTest do
  use ExUnit.Case
  doctest Extras

  describe "range" do
    test "returns an empty list for descending sequences" do
      assert Extras.range(0, -2) == []
    end

    test "returns an empty list for non-ascending sequences" do
      assert Extras.range(8, 8) == []
    end

    test "returns an exclusive range" do
      assert Extras.range(3, 6) == [3, 4, 5]
    end
  end
end
