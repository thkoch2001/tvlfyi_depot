defmodule Server.MixProject do
  use Mix.Project

  def project do
    [
      app: :server,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {App, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:cortex, "~> 0.1", only: [:dev, :test]},
      {:plug_cowboy, "~> 2.4.1"},
      {:cowboy, "~> 2.8.0"},
      {:plug, "~> 1.11.0"},
      {:poison, "~> 4.0.1"}
    ]
  end
end
