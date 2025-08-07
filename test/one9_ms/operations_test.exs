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

  def t0(value) do
    one_of([
      t(value, strict: false),
      list_of(tuple({value, non_negative_integer()})),
      mapset_of(tuple({value, non_negative_integer()})), # representative arbitrary non-list non-map enumerable
    ])
  end

  defp enumerable(value, options) do
    case Keyword.pop(options, :finite, false) do
      {true, []} ->
        one_of([
          list_of(value),
          mapset_of(value), # arbitrary enumerable struct
          One9.MultisetTest.t(value), # arbitrary enumerable struct
          map(list_of(value), &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)), # arbitrary enumerable struct
        ])

      {false, []} ->
        one_of([
          map(value, &Stream.cycle/1),
          enumerable(value, finite: true)
        ])

      {_, options} ->
        raise RuntimeError, "unsupported options: #{inspect Keyword.keys(options)}"
    end
  end

  test "counts default" do
    assert One9.Ms.equals?(One9.Ms.counts(), %{})
  end

  property "counts always returns a well-formed multiset" do
    check all \
      ms <- map(enumerable(term(), finite: true), &One9.Ms.counts/1)
    do
      assert One9.Ms.well_formed?(ms)
    end
  end

  property "from_counts always returns a well-formed multiset" do
    check all \
      ms <- map(t0(term()), &One9.Ms.from_counts/1)
    do
      assert One9.Ms.well_formed?(ms)
    end
  end

  property "well_formed basic correctness" do
    check all \
      ms <- t(term(), strict: false)
    do
      if 0 in Map.values(ms) do
        refute One9.Ms.well_formed?(ms)
      else
        assert One9.Ms.well_formed?(ms)
      end
    end
  end

  test "from_counts default" do
    assert One9.Ms.equals?(
      One9.Ms.from_counts(),
      %{},
      :strict
    )
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

  property "delete returns a well-formed multiset whenever input is well-formed" do
    check all \
      ms1 <- t(term(), strict: true),
      element <- if(One9.Ms.empty?(ms1), do: term(), else: one_of([term(), member_of(Map.keys(ms1))])),
      count <- one_of([non_negative_integer(), constant(:all), constant(nil)]),
      ms2 = case(count, do: (nil -> One9.Ms.delete(ms1, element); count -> One9.Ms.delete(ms1, element, count)))
    do
      assert One9.Ms.well_formed?(ms2)
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
    refute is_non_neg_integer(-1)

    assert is_pos_integer(1)
    refute is_pos_integer(0)
    refute is_pos_integer(-1)

    assert is_non_struct_map(%{})
    refute is_non_struct_map(MapSet.new([]))
    refute is_non_struct_map(42)
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
        {start, _} = :gb_trees.smallest(tree)
        assert Map.new(gbt_iter_from(tree, start)) === map
      end
    end
  end
end
