defmodule One9.MsTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  doctest One9.Ms, import: true

  import One9.Ms.Util
  require One9.Ms.Util
  import One9.MsTest.Util

  test "counts/0 returns the empty multiset" do
    assert One9.Ms.counts() === %{}
  end

  property "counts/1 result always strict" do
    check all enumerable <- enumerable(term(), finite: true) do
      assert One9.Ms.strict?(One9.Ms.counts(enumerable))
    end
  end

  property "counts/1 inverts One9.Multiset.new/1" do
    check all ms <- t(term(), strict: false) do
      assert One9.Ms.equals?(One9.Ms.counts(One9.Multiset.new(ms)), ms)
    end

    check all ms <- t(term(), strict: true) do
      assert One9.Ms.equals?(One9.Ms.counts(One9.Multiset.new(ms)), ms, :strict)
    end
  end

  test "delete/2 deletes 1 copy" do
    check all ms <- t(term(), strict: false) do
      check all value <- one_of_([One9.Ms.support(ms), term()]) do
        assert One9.Ms.equals? \
          One9.Ms.delete(ms, value),
          One9.Ms.delete(ms, value, 1)
      end
    end
  end

  property "delete/3 result strict whenever inputs are strict" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        assert One9.Ms.strict?(One9.Ms.delete(ms, value, count))
      end
    end
  end

  property "delete/4 lax result preserves all keys from input" do
    check all ms <- t(term(), strict: false) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        result = One9.Ms.delete(ms, value, count, :lax)

        assert Enum.all?(Map.keys(ms), &Map.has_key?(result, &1))
      end
    end
  end

  property "delete/4 strict result always strict" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        assert One9.Ms.strict?(One9.Ms.delete(ms, value, count, :strict))
      end
    end
  end

  property "difference/2 does not raise when right is not a subset of left" do
    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: false) do
      One9.Ms.difference(ms1, ms2)
    end
  end

  property "difference/3 does not raise when right is not a subset of left" do
    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: false) do
      One9.Ms.difference(ms1, ms2, :lax)
    end

    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: true) do
      One9.Ms.difference(ms1, ms2, :strict)
    end
  end

  property "difference/2 result strict whenever inputs are strict" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.strict?(One9.Ms.difference(ms1, ms2))
    end
  end

  property "difference/3 lax result preserves all keys from left input" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.difference(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
    end
  end

  property "difference/3 strict result always strict" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.strict?(One9.Ms.difference(ms1, ms2, :strict))
    end
  end

  property "difference!/3 raises when right is not a subset of left" do
    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: false) do
      assert_raise KeyError, fn -> One9.Ms.difference!(ms1, ms2) end
    end
  end

  property "difference!/4 raises when right is not a subset of left" do
    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: false) do
      assert_raise KeyError, fn -> One9.Ms.difference!(ms1, ms2, :lax) end
    end

    check all {ms1, ms2} <- t_and_nonsubset(term(), strict: true) do
      assert_raise KeyError, fn -> One9.Ms.difference!(ms1, ms2, :strict) end
    end
  end

  property "difference! result strict whenever left input is strict" do
    check all {ms1, ms2} <- (
      t_and_subset(term(), t_strict: false)
      |> map(fn {ms1, ms2} -> {One9.Ms.from_counts(ms1), ms2} end)
    ) do
      assert One9.Ms.strict?(One9.Ms.difference!(ms1, ms2))
    end
  end

  property "difference! lax result preserves all keys from left input" do
    check all {ms1, ms2} <- t_and_subset(term(), t_strict: false) do
      result = One9.Ms.difference!(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
    end
  end

  property "difference! strict result always strict" do
    check all {ms1, ms2} <- t_and_subset(term()) do
      assert One9.Ms.strict?(One9.Ms.difference!(ms1, ms2, :strict))
    end
  end

  test "from_counts/0 returns the empty multiset" do
    assert One9.Ms.from_counts() === %{}
  end

  property "from_counts/1 always returns a strict result" do
    check all counts <- t0(term()) do
      assert One9.Ms.strict?(One9.Ms.from_counts(counts))
    end
  end

  test "put/2 puts 1 copy" do
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

  property "put/3 returns a strict multiset whenever input is strict" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        non_negative_integer()
      }) do
        assert One9.Ms.strict?(One9.Ms.put(ms, value, count))
      end
    end
  end

  property "put/4 lax result preserves all keys from input" do
    check all ms <- t(term(), strict: false) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        non_negative_integer()
      }) do
        result = One9.Ms.put(ms, value, count, :lax)

        assert Enum.all?(Map.keys(ms), &Map.has_key?(result, &1))
      end
    end
  end

  property "put/4 strict result always strict" do
    check all ms <- t(term(), strict: true) do
      check all {value, count} <- tuple({
        one_of_([One9.Ms.support(ms), term()]),
        one_of([non_negative_integer(), :default!])
      }) do
        result = case count do
          :default! -> One9.Ms.put(ms, value, :strict)
          count -> One9.Ms.put(ms, value, count, :strict)
        end

        assert One9.Ms.strict?(result)
      end
    end
  end

  test "strict?/1 accepts strict inputs" do
    assert One9.Ms.strict?(%{})
    assert One9.Ms.strict?(%{42 => 1})
    assert One9.Ms.strict?(%{nil => 1})
    assert One9.Ms.strict?(%{0 => 1})

    check all ms <- t(term(), strict: true) do
      assert One9.Ms.strict?(ms)
    end
  end

  test "strict?/1 rejects non-strict inputs" do
    refute One9.Ms.strict?(%{42 => 1, 43 => 0})
    refute One9.Ms.strict?(%{nil => 0})
    refute One9.Ms.strict?(%{true => 1, false: 0})

    check all ms <- t(term(), strict: :never) do
      refute One9.Ms.strict?(ms)
    end
  end

  test "strict?/1 raises on bad multisets" do
    assert_raise ArgumentError, fn -> One9.Ms.strict?(42) end
    assert_raise ArgumentError, fn -> One9.Ms.strict?(nil) end
    assert_raise ArgumentError, fn -> One9.Ms.strict?([]) end
    assert_raise ArgumentError, fn -> One9.Ms.strict?(%{43 => 0.0}) end
    assert_raise ArgumentError, fn -> One9.Ms.strict?(%{43 => -1}) end

    check all bad <- not_t() do
      assert_raise ArgumentError, fn -> One9.Ms.strict?(bad) end
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

  property "symmetric_difference result strict whenever inputs are strict" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.strict?(One9.Ms.symmetric_difference(ms1, ms2))
    end
  end

  property "symmetric_difference strict result always strict" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.strict?(One9.Ms.symmetric_difference(ms1, ms2, :strict))
    end
  end

  property "symmetric_difference lax result preserves all keys from inputs" do
    check all ms1 <- t(term(), strict: false), ms2 <- t(term(), strict: false) do
      result = One9.Ms.symmetric_difference(ms1, ms2, :lax)

      assert Enum.all?(Map.keys(ms1), &Map.has_key?(result, &1))
      assert Enum.all?(Map.keys(ms2), &Map.has_key?(result, &1))
    end
  end

  property "union result strict whenever inputs are strict" do
    check all ms1 <- t(term(), strict: true), ms2 <- t(term(), strict: true) do
      assert One9.Ms.strict?(One9.Ms.union(ms1, ms2))
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
