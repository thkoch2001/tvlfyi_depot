defmodule App do
  use Application

  @impl true
  def start(_type, _args) do
    Sup.start_link()
  end
end
