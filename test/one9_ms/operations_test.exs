defmodule One9.MsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest One9.Ms

  def t(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {strict, []} when not strict ->
        map_of(value, non_negative_integer())

      {strict, []} when strict ->
        map_of(value, positive_integer())

      {_, options} ->
        raise RuntimeError, "unsupported options: #{inspect Keyword.keys(options)}"
    end
  end

  test "counts default" do
    assert One9.Ms.equals?(One9.Ms.counts(), %{})
  end

  test "delete default" do
    assert (%{"rat" => 999}
            |> One9.Ms.delete("rat")
            |> One9.Ms.count_element("rat"))
           === 0
  end

  test "from_counts default" do
    assert One9.Ms.equals?(One9.Ms.from_counts(), %{})
  end

  test "put default" do
    assert One9.Ms.equals?(
      %{"dog" => 3, "cat" => 1} |> One9.Ms.put("cat"),
      %{"dog" => 3, "cat" => 2},
      :strict
    )
  end

  property "struct interop" do
    check all \
      ms <- t(term()),
      multiset = One9.Multiset.new(ms)
    do
      assert One9.Ms.equals?(One9.Ms.counts(multiset), ms)
    end
  end
end

defmodule One9.MsUtilTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import One9.Ms.Util
  require One9.Ms.Util

  test "guards" do
    assert is_non_neg_integer(1)
    assert is_non_neg_integer(0)
    assert not is_non_neg_integer(-1)

    assert is_pos_integer(1)
    assert not is_pos_integer(0)
    assert not is_pos_integer(-1)

    assert is_non_struct_map(%{})
    assert not is_non_struct_map(MapSet.new([]))
    assert not is_non_struct_map(42)
  end

  property "to_gbt round-trip with map" do
    check all \
      map <- map_of(term(), term()),
      tree <- map(one_of([
        constant(map),
        constant(One9.Ms.Util.map_iter(map)),
      ]), &One9.Ms.Util.to_gbt/1)
    do
      assert Map.new(:gb_trees.to_list(tree)) === map
      if not :gb_trees.is_empty(tree) do
        {key, _} = :gb_trees.smallest(tree)
        assert Map.new(gbt_iter_from(tree, key)) === map
      end
    end
  end
end
