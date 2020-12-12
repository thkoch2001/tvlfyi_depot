defmodule Cache do
  @moduledoc """
  Cache is an in-memory key-value store.
  """
  use Agent

  @doc """
  Inititalize the key-value store.
  """
  def start_link(_) do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @doc """
  Attempt to return the value stored at `key`
  """
  def get(key) do
    Agent.get(__MODULE__, &Map.get(&1, key))
  end

  @doc """
  Write the `value` under the `key`. Last writer wins.
  """
  def put(key, value) do
    Agent.update(__MODULE__, &Map.put(&1, key, value))
  end
end
