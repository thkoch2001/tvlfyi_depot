defmodule Extras do
  @moduledoc """
  Hosts utility functions intended to supplement the standard library.
  """

  @doc """
  Return an ascending range starting at `a` and ending at `b` (exclusive).

  ## Examples

      iex> Extras.range(2, 5)
      [2, 3, 4]

  """
  def range(a, b) do
    if b <= a do
      []
    else
      [a] ++ range(a + 1, b)
    end
  end
end
