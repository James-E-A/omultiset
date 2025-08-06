defmodule One9.Ms.MixProject do
  use Mix.Project

  def project do
    [
      app: :omultiset,
      version: "0.2.0",
      elixir: "~> 1.17", # FIXME: check what actual minimum version works
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  defp deps do [
    {:stream_data, "~> 1.0", only: :test},
  ] end
end
