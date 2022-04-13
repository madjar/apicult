defmodule Apicult.MixProject do
  use Mix.Project

  @name "Apicult"
  @repo_url "https://github.com/madjar/apicult"

  def project do
    [
      app: :apicult,
      version: "0.2.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      description: "A nice api description language that allow for quick api integration",
      package: package(),
      docs: docs(),
      deps: deps(),
      name: @name,
      source_url: @repo_url,
      homepage_url: @repo_url
    ]
  end

  # Include tests in compilation, to avoid issue with protocol consolidation
  defp elixirc_paths(:test), do: ["lib", "test"]
  defp elixirc_paths(_), do: ["lib"]

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

  defp package do
    [
      licenses: ["Anti-Capitalist Software License"],
      links: %{"GitHub" => @repo_url}
    ]
  end

  defp docs do
    [main: "Apicult", source_url_pattern: "#{@repo_url}/blob/main/elixir/%{path}#L%{line}"]
  end
end
