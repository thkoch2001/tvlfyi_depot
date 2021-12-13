defmodule Server do
  @moduledoc """
  Documentation for `Server`.
  """

  @doc """
  If `n` contains exactly two prime factors, return those prime factors;
  otherwise, return nothing.
  """
  def semiprime(n) do
    case Cache.get(n) do
      nil ->
        case do_semiprime(n) do
          nil ->
            nil

          res ->
            Cache.put(n, res)
            {:miss, res}
        end

      hit ->
        {:hit, hit}
    end
  end

  defp do_semiprime(n) do
    case Math.factor(n) do
      [_, _] = res -> res
      _ -> nil
    end
  end
end
