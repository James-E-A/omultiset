defmodule One9.MsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest One9.Ms

  def t(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        map_of(value, non_negative_integer())

      {true, []} ->
        map_of(value, positive_integer())

      {:never, []} ->
        tuple({t(value, strict: true), value})
        |> map(fn {ms, canary} -> Map.put(ms, canary, 0) end)

      {_, options} ->
        raise RuntimeError, "unsupported options: #{inspect Keyword.keys(options)}"
    end
  end

  def t_and_subset(value, options \\ []) do
    tuple({t(value, options), t(value, options)})
    |> map(fn {ms1, ms2} -> {One9.Ms.union(ms1, ms2), ms2} end)
  end

  def t_and_strict_subset(value, options \\ []) do
    tuple({nonempty(t(value, options)), t(value, options)})
    |> map(fn {ms1, ms2} -> {One9.Ms.sum(ms1, ms2), ms2} end)
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
          StreamData.mapset_of(value), # arbitrary enumerable struct
          ##One9.MultisetTest.t(value), # arbitrary enumerable struct
          StreamData.map(list_of(value), &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)), # arbitrary enumerable struct
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

  defmacrop implies(a, b) do
    quote do: not (unquote(a) and not unquote(b))
  end

  test "counts default" do
    result = One9.Ms.counts()

    assert result === %{}
    assert One9.Ms.equals? result, %{}, :strict
  end

  property "counts always returns a well-formed multiset" do
    check all enumerable <- enumerable(term(), finite: true) do
      result = One9.Ms.counts(enumerable)

      assert One9.Ms.well_formed?(result)
      assert Enum.all?(result, fn {_, n} when is_integer(n) -> n > 0; _ -> false end)
    end
  end

  property "from_counts always returns a well-formed multiset" do
    check all \
      counts <- t0(term())
    do
      result = One9.Ms.from_counts(counts)

      assert One9.Ms.well_formed?(result)
      assert Enum.all?(result, fn {_, n} when is_integer(n) -> n > 0; _ -> false end)
    end
  end

  property "well_formed basic correctness" do
    assert One9.Ms.well_formed?(%{})
    refute One9.Ms.well_formed?(%{42 => 0})

    check all \
      ms <- t(term(), strict: false)
    do
      assert One9.Ms.well_formed?(ms) === (0 not in Map.values(ms))
    end
  end

  test "from_counts default" do
    result = One9.Ms.from_counts()

    assert result === %{}
    assert One9.Ms.equals? result, %{}, :strict
  end

  test "put default" do
    assert One9.Ms.equals? \
      %{"dog" => 3, "cat" => 1} |> One9.Ms.put("cat"),
      %{"dog" => 3, "cat" => 2},
      :strict
  end

  property "put default form returns a well-formed multiset whenever input is well-formed" do
    check all ms <- t(term(), strict: false) do
      check all value <- term(), count <- one_of([non_negative_integer(), :default!]) do
        result = case count do
          :default! -> One9.Ms.put(ms, value)
          count -> One9.Ms.put(ms, value, count)
        end

        assert implies One9.Ms.well_formed?(ms), One9.Ms.well_formed?(result)
      end
    end
  end

  property "put lax form never prunes entries from input" do
    check all ms <- t(term(), strict: :never), not One9.Ms.well_formed?(ms) do
      check all value <- term(), count <- one_of([non_negative_integer(), :default!]) do
        result = case count do
          :default! -> One9.Ms.put(ms, value, :lax)
          count -> One9.Ms.put(ms, value, count, :lax)
        end

        assert Enum.all?(ms, fn
          {element, _} ->
            Map.has_key?(result, element)
        end)
      end
    end
  end

  property "put strict form never returns non-strict" do
    check all ms <- t(term(), strict: true) do
      check all value <- term(), count <- one_of([non_negative_integer(), :default!]) do
        result = case count do
          :default! -> One9.Ms.put(ms, value, :strict)
          count -> One9.Ms.put(ms, value, count, :strict)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "struct interop" do
    check all ms <- t(term()) do
      assert One9.Ms.equals? One9.Ms.counts(One9.Multiset.new(ms)), ms
    end

    check all ms <- t(term(), strict: true) do
      assert One9.Ms.equals? One9.Ms.counts(One9.Multiset.new(ms)), ms, :strict
    end
  end

  property "delete default form returns a well-formed multiset whenever input is well-formed" do
    check all ms <- t(term(), strict: false) do
      check all value <- term(), count <- one_of([non_negative_integer(), :all, :default!]) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value)
          count -> One9.Ms.delete(ms, value, count)
        end

        assert implies One9.Ms.well_formed?(ms), One9.Ms.well_formed?(result)
      end
    end
  end

  property "delete lax form never prunes entries from input" do
    check all ms <- t(term(), strict: :never), not One9.Ms.well_formed?(ms) do
      check all value <- term(), count <- one_of([non_negative_integer(), :all, :default!]) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value, :lax)
          count -> One9.Ms.delete(ms, value, count, :lax)
        end

        assert Enum.all?(ms, fn
          {element, _} ->
            Map.has_key?(result, element)
        end)
      end
    end
  end

  property "delete strict form never returns non-strict" do
    check all ms <- t(term(), strict: true) do
      check all value <- term(), count <- one_of([non_negative_integer(), :all, :default!]) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value, :strict)
          count -> One9.Ms.delete(ms, value, count, :strict)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "union default form returns a well-formed multiset whenever both inputs are well-formed" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.union(ms1, ms2)

      assert implies One9.Ms.well_formed?(ms1) and One9.Ms.well_formed?(ms2),
        One9.Ms.well_formed?(result)
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
