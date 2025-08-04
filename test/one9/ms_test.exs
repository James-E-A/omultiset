defmodule One9.MsTest do
  use ExUnit.Case
  doctest One9.Ms

  test "from List" do
    assert One9.Ms.from_elements([:hello, :world, :world]) == %{hello: 1, world: 2}
  end

  test "from MapSet" do
    assert One9.Ms.from_elements(MapSet.new([:hello, :world, :world])) == %{hello: 1, world: 1}
  end
end
