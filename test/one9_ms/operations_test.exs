defmodule One9.MsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest One9.Ms, import: true

  def t(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        map_of(value, non_negative_integer())

      {true, []} ->
        map_of(value, positive_integer())

      {:never, []} ->
        tuple({t(value, strict: false), value})
        |> map(fn {ms, canary} -> Map.put(ms, canary, 0) end)

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

  defp t_with_subset(value, options \\ []) do
    {strict, options} = Keyword.pop(options, :strict, false)
    {t_strict, options} = Keyword.pop(options, :t_strict, true)
    options = Keyword.put(options, :strict, t_strict)

    if not strict do
      tuple({t(value, options), t(value, options)})
      |> map(fn {ms1, ms2} ->
        {One9.Ms.union(ms1, ms2), ms2}
      end)

    else
      tuple({nonempty(t(value, options)), t(value, options)})
      |> map(fn {ms1, ms2} ->
        {One9.Ms.sum(ms1, ms2), ms2}
      end)
    end
  end

  defp t_with_non_subset(value) do
    tuple({t(value), t(value)})
    |> filter(fn {ms1, ms2} -> not One9.Ms.subset?(ms2, ms1) end)
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

  defp one_of_(datas_and_enumerables) do
    datas_and_enumerables
    |> Enum.map(fn
      %StreamData{} = data -> data
      enumerable -> if Enum.empty?(enumerable), do: nil, else: StreamData.member_of(enumerable)
    end)
    |> Enum.filter(& &1)
    |> one_of()
  end

  test "counts default" do
    result = One9.Ms.counts()

    assert result === %{}
    assert One9.Ms.equals?(result, %{}, :strict)
  end

  property "counts always returns a well-formed result" do
    check all enumerable <- enumerable(term(), finite: true) do
      result = One9.Ms.counts(enumerable)

      assert One9.Ms.well_formed?(result)
      assert Enum.all?(result, fn {_, n} when is_integer(n) -> n > 0; _ -> false end)
    end
  end

  property "from_counts always returns a well-formed result" do
    check all counts <- t0(term()) do
      result = One9.Ms.from_counts(counts)

      assert One9.Ms.well_formed?(result)
      assert Enum.all?(result, fn {_, n} when is_integer(n) -> n > 0; _ -> false end)
    end
  end

  property "well_formed? basic correctness" do
    assert One9.Ms.well_formed?(%{})
    refute One9.Ms.well_formed?(%{42 => 0})

    check all ms <- t(term(), strict: false) do
      assert One9.Ms.well_formed?(ms) === (0 not in Map.values(ms))
    end
  end

  test "from_counts empty by default" do
    result = One9.Ms.from_counts()

    assert result === %{}
    assert One9.Ms.equals?(result, %{}, :strict)
  end

  test "put 1 by default" do
    assert One9.Ms.equals? \
      %{"dog" => 3, "cat" => 1} |> One9.Ms.put("cat"),
      %{"dog" => 3, "cat" => 2},
      :strict

      check all ms <- t(term(), strict: false) do
        check all value <- one_of_([One9.Ms.support(ms), term()]) do
          result = One9.Ms.put(ms, value)

          refute One9.Ms.equals?(result, ms)
          assert One9.Ms.equals?(result, One9.Ms.put(ms, value, 1))
        end
      end
  end

  property "put default form returns a well-formed multiset whenever input is well-formed" do
    check all ms <- t(term(), strict: true) do
      check all value <- one_of_([One9.Ms.support(ms), term()]),
                count <- one_of([non_negative_integer(), :default!]) do
        result = case count do
          :default! -> One9.Ms.put(ms, value)
          count -> One9.Ms.put(ms, value, count)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "put lax result preserves all keys from input" do
    check all ms <- t(term(), strict: false) do
      check all value <- one_of_([One9.Ms.support(ms), term()]),
                count <- one_of([non_negative_integer(), :default!]) do
        result = case count do
          :default! -> One9.Ms.put(ms, value, :lax)
          count -> One9.Ms.put(ms, value, count, :lax)
        end

        assert Enum.all?(Map.keys(ms), &Map.has_key?(result, &1))
      end
    end
  end

  property "put strict result always well-formed" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([non_negative_integer(), :default!])
      }) do
        result = case count do
          :default! -> One9.Ms.put(ms, value, :strict)
          count -> One9.Ms.put(ms, value, count, :strict)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "struct interop" do
    check all ms <- t(term(), strict: false) do
      assert One9.Ms.equals?(One9.Ms.counts(One9.Multiset.new(ms)), ms)
    end

    check all ms <- t(term(), strict: true) do
      assert One9.Ms.equals?(One9.Ms.counts(One9.Multiset.new(ms)), ms, :strict)
    end
  end

  property "delete result well-formed whenever inputs are well-formed" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer(), :default!])
      }) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value)
          count -> One9.Ms.delete(ms, value, count)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "delete lax result preserves all keys from input" do
    check all ms <- t(term(), strict: false) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer(), :default!])
      }) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value, :lax)
          count -> One9.Ms.delete(ms, value, count, :lax)
        end

        assert Enum.all?(Map.keys(ms), &Map.has_key?(result, &1))
      end
    end
  end

  property "delete strict result always well-formed" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer(), :default!])
      }) do
        result = case count do
          :default! -> One9.Ms.delete(ms, value, :strict)
          count -> One9.Ms.delete(ms, value, count, :strict)
        end

        assert One9.Ms.well_formed?(result)
      end
    end
  end

  property "difference does not raise when right is not a subset of left" do
    check all {ms1, ms2} <- t_with_non_subset(term()) do
      One9.Ms.difference(ms1, ms2)
    end
  end

  property "difference result well-formed whenever inputs are well-formed" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.well_formed?(One9.Ms.difference(ms1, ms2))
    end
  end

  property "difference lax result preserves all keys from left input" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.difference(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
    end
  end

  property "difference strict result always well-formed" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.well_formed?(One9.Ms.difference(ms1, ms2, :strict))
    end
  end

  property "difference! raises when right is not a subset of left" do
    check all {ms1, ms2} <- t_with_non_subset(term()) do
      assert_raise KeyError, fn ->
        One9.Ms.difference!(ms1, ms2)
      end
    end
  end

  property "difference! result well-formed whenever left input is well-formed" do
    check all {ms1, ms2} <- (
      t_with_subset(term(), t_strict: false)
      |> map(fn {ms1, ms2} -> {One9.Ms.from_counts(ms1), ms2} end)
    ) do
      assert One9.Ms.well_formed?(One9.Ms.difference!(ms1, ms2))
    end
  end

  property "difference! lax result preserves all keys from left input" do
    check all {ms1, ms2} <- t_with_subset(term(), t_strict: false) do
      result = One9.Ms.difference!(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
    end
  end

  property "difference! strict result always well-formed" do
    check all {ms1, ms2} <- t_with_subset(term()) do
      assert One9.Ms.well_formed?(One9.Ms.difference!(ms1, ms2, :strict))
    end
  end

  property "union result well-formed whenever inputs are well-formed" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.well_formed?(One9.Ms.union(ms1, ms2))
    end
  end

  property "symmetric_difference basic correctness" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.symmetric_difference(ms1, ms2)

      check all value <- one_of_([One9.Ms.support(ms1), One9.Ms.support(ms2), term()]) do
        assert One9.Ms.count_element(result, value) ===
          abs(
            One9.Ms.count_element(ms1, value) -
            One9.Ms.count_element(ms2, value)
          )
      end
    end
  end

  property "symmetric_difference result well-formed whenever inputs are well-formed" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.well_formed?(One9.Ms.symmetric_difference(ms1, ms2))
    end
  end

  property "symmetric_difference strict result always well-formed" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.well_formed?(One9.Ms.symmetric_difference(ms1, ms2, :strict))
    end
  end

  property "symmetric_difference lax result preserves all keys from inputs" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.symmetric_difference(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
      assert Enum.all?(Map.keys(ms2), &Map.has_key?(result, &1))
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
    check all map <- map_of(term(), term()) do
      assert Map.new(:gb_trees.to_list(One9.Ms.Util.to_gbt(map))) === map
    end
  end
end
