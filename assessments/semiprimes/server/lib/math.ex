defmodule Math do
  @moduledoc """
  Math utilities.
  """
  alias Extras

  @doc """
  Returns the prime factors for `n`.

  ## Examples

      iex> Math.factor(15)
      [3, 5]

  """
  def factor(1), do: []

  def factor(n) do
    Extras.range(2, n - 1)
    |> Enum.find(&(rem(n, &1) == 0))
    |> case do
      nil -> [n]
      x -> [x | factor(div(n, x))]
    end
  end
end
