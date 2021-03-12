defmodule GenNntp.MixProject do
  use Mix.Project

  @version "0.18.0"

  def project do
    [
      app: :gen_nntp,
      version: @version,
      name: "GenNNTP",
      source_url: "https://github.com/sntran/gen_nntp",
      homepage_url: "https://sntran.github.io/gen_nntp",
      description: """
      The NNTP client and server library.
      """,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs(),
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ranch, "~> 2.0.0"},
      {:mix_test_watch, "~> 1.0", only: :dev, runtime: false},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false},
    ]
  end

  defp package do
    [
      maintainers: [
        "Son Tran-Nguyen"
      ],
      licenses: ["Apache 2.0"],
      links: %{github: "https://github.com/sntran/gen_nntp"},
      files:
        ~w(examples include lib priv src) ++
          ~w(.formatter.exs mix.exs CHANGELOG.md LICENSE README.md),
      exclude_patterns: [".DS_Store"]
    ]
  end

  defp docs do
    [
      main: "GenNNTP",
      source_ref: "v#{@version}"
    ]
  end
end
