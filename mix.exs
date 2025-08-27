defmodule One9.Ms.MixProject do
  use Mix.Project

  def project do
    [
      app: :omultiset,
      version: "0.4.0-rc.0",
      elixir: "~> 1.17", # FIXME: check what actual minimum version works
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps()
    ]
  end

  defp deps do [
    {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
    {:stream_data, "~> 1.0", only: [:test, :dev]},
    {:benchee, "~> 1.0", only: :dev, runtime: false},
  ] end

  defp package do [
    name: "omultiset",
    description: "Highly efficient Multiset library. A struct with complete `Enumerable` and `Inspect` implementations, plus utilities to manipulate \"plain\" multiplicity maps.",
    licenses: ["BSD-3-Clause"],
    links: %{"source" => "https://github.com/James-E-A/omultiset"},
  ] end
end
