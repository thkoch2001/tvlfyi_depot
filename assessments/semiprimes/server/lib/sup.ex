defmodule Sup do
  @moduledoc """
  Top-level supervisor for our OTP application. For now, this supervisor starts
  and monitors our cache.
  """

  use Supervisor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      Cache
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
