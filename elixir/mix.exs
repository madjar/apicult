defmodule Apicult.MixProject do
  use Mix.Project

  def project do
    [
      app: :apicult,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "Apicult",
      source_url: "https://github.com/madjar/apicult",
      homepage_url: "https://github.com/madjar/apicult"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Apicult.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:finch, "~> 0.10"},
      {:jason, "~> 1.3"},
      {:ex_doc, "~> 0.26", only: :dev, runtime: false}
    ]
  end
end
