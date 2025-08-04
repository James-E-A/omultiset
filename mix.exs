defmodule One9.Ms.MixProject do
  use Mix.Project

  def project do
    [
      app: :omultiset,
      version: "0.1.1",
      elixir: "~> 1.17", # FIXME: check what actual minimum version works
      start_permanent: Mix.env() == :prod,
      deps: []
    ]
  end
end
