defmodule One9.MultisetTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  doctest One9.Multiset

  import One9.MsTest.Util, only: [one_of_: 1, range_within: 1]

  test "inspect" do
    assert inspect(One9.Multiset.new()) === "One9.Multiset.new([])"
    assert inspect(One9.Multiset.new([1, 2, 3, 3, 3])) === "One9.Multiset.new([1, 2, 3, 3, 3])"
    assert inspect(One9.Multiset.new(%{42 => 10**100})) === "One9.Multiset.new(%{42 => #{10**100}})"
  end

  def t(value) do
    # don't bother stress-testing with lax inputs; that's done in a separate test
    One9.MsTest.Util.t(value, strict: true)
    |> map(&One9.Multiset.new/1)
  end
  def t(), do: t(term())

  defp t_and_subset(value, options) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        tuple({t(value), t(value)})
        |> map(fn {mset1, mset2} ->
          {One9.Multiset.union(mset1, mset2), mset2}
        end)

      {true, []} ->
        tuple({nonempty(t(value)), t(value)})
        |> map(fn {mset1, mset2} ->
          {One9.Multiset.sum(mset1, mset2), mset2}
        end)
    end
  end
  def t_and_subset(options_or_value \\ [])
  def t_and_subset(options) when is_list(options), do: t_and_subset(term(), options)
  def t_and_subset(value), do: t_and_subset(value, [])

  #doc "https://en.wikipedia.org/wiki/Material_conditional"
  defmacrop implies(a, b) do
    quote do: not (unquote(a) and not unquote(b))
  end

  property "new/1 idempotence" do
    check all mset <- t() do
      assert One9.Multiset.equals?(One9.Multiset.new(mset), mset)
    end
  end

  property "new/1 inverts to_list/1" do
    check all mset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.new(One9.Multiset.to_list(mset)),
        mset
    end
  end

  property "new/1 inverts to_counts/1" do
    check all mset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.new(One9.Multiset.to_counts(mset)),
        mset
    end
  end

  property "count_element/2 agrees with Enum.count/2" do
    check all mset <- t() do
      list = One9.Multiset.to_list(mset)
      check all value <- one_of_([One9.Multiset.support(mset), term()]) do
        assert One9.Multiset.count_element(mset, value) ===
          Enum.count(list, &(&1 === value))
      end
    end
  end

  property "difference/2 preserves length" do
    check all mset1 <- t(), mset2 <- t() do
      assert One9.Multiset.size(One9.Multiset.difference(mset1, mset2)) <=
        One9.Multiset.size(mset1)
    end
  end

  property "difference!/2 preserves length" do
    check all {mset1, mset2} <- t_and_subset() do
      assert One9.Multiset.size(One9.Multiset.difference!(mset1, mset2)) <=
        One9.Multiset.size(mset1)
    end
  end

  property "empty/1 basic correctness" do
    assert One9.Multiset.empty?(One9.Multiset.new([]))
    refute One9.Multiset.empty?(One9.Multiset.new([[]]))
    refute One9.Multiset.empty?(One9.Multiset.new([nil]))
    refute One9.Multiset.empty?(One9.Multiset.new([0]))

    assert One9.Multiset.empty?(One9.Multiset.new(%{}))
    assert One9.Multiset.empty?(One9.Multiset.new(%{42 => 0}))
    refute One9.Multiset.empty?(One9.Multiset.new(%{42 => 1}))

    check all mset <- t() do
      assert One9.Multiset.empty?(mset) ===
        (One9.Multiset.size(mset) === 0)
    end
  end

  property "equals?/2 is preserved by Enum.shuffle/1" do
    check all list <- list_of(term()) do
      assert One9.Multiset.equals? \
        One9.Multiset.new(Enum.shuffle(list)),
        One9.Multiset.new(list)
    end

    check all ms <- One9.MsTest.Util.t() do
      assert One9.Multiset.equals? \
        One9.Multiset.from_counts(Enum.shuffle(ms)),
        One9.Multiset.new(ms)
    end

    check all counts <- One9.MsTest.Util.t0() do
      assert One9.Multiset.equals? \
        One9.Multiset.from_counts(Enum.shuffle(counts)),
        One9.Multiset.from_counts(counts)
    end
  end

  property "equals?/2 is NOT preserved by put/2" do
    check all mset <- t(), value <- term() do
      refute One9.Multiset.equals? \
        One9.Multiset.put(mset, value),
        mset
    end
  end

  property "equals?/2 is preserved by put/3 with 0 quantity" do
    check all mset <- t(), value <- term() do
      assert One9.Multiset.equals? \
        One9.Multiset.put(mset, value, 0),
        mset
    end
  end

  property "equals?/2 is NOT preserved by put/3 with positive quantity" do
    check all mset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(mset), term()]),
        positive_integer()
      }) do
        refute One9.Multiset.equals? \
          One9.Multiset.put(mset, value, count),
          mset
      end
    end
  end

  property "from_elements/1 preserves Enum.member?/2" do
    check all list <- list_of(term()) do
      result = One9.Multiset.from_elements(list)

      check all value <- term() do
        assert Enum.member?(result, value) === Enum.member?(list, value)
      end
    end
  end

  property "to_list/1 inverts from_elements/1" do
    check all list <- list_of(term()) do
      assert Enum.sort(One9.Multiset.to_list(One9.Multiset.from_elements(list))) ===
        Enum.sort(list)
    end
  end

  property "from_elements/1 inverts to_list/1" do
    check all mset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.from_elements(One9.Multiset.to_list(mset)),
        mset
    end
  end

  property "to_list/1 preserves Enum.slice/2" do
    check all mset <- t() do
      check all range <- range_within(mset) do
        assert Enum.slice(mset, range) ===
          Enum.slice(One9.Multiset.to_list(mset), range)
      end
    end
  end

  property "intersection/2 is commutative" do
    check all mset1 <- t(), mset2 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(mset1, mset2),
        One9.Multiset.intersection(mset2, mset1)
    end
  end

  property "intersection/2 is associative" do
    check all mset1 <- t(), mset2 <- t(), mset3 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(One9.Multiset.intersection(mset1, mset2), mset3),
        One9.Multiset.intersection(mset1, One9.Multiset.intersection(mset2, mset3))
    end
  end

  property "intersection/2 is idempotent" do
    check all mset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(mset, mset),
        mset
    end
  end

  property "intersection/2 preserves size/1" do
    check all mset1 <- t(), mset2 <- t() do
      result = One9.Multiset.intersection(mset1, mset2)

      assert One9.Multiset.size(result) <= One9.Multiset.size(mset1)
      assert One9.Multiset.size(result) <= One9.Multiset.size(mset2)
    end
  end

  property "intersection/2 produces subsets" do
    check all mset1 <- t(), mset2 <- t() do
      result = One9.Multiset.intersection(mset1, mset2)

      assert One9.Multiset.subset?(result, mset1)
      assert One9.Multiset.subset?(result, mset2)
    end
  end

  property "put/3 basic correctness" do
    check all mset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(mset), term()]),
        non_negative_integer()
      }) do
        assert One9.Multiset.count_element(One9.Multiset.put(mset, value, count), value) ===
          One9.Multiset.count_element(mset, value) + count
      end
    end
  end

  property "put/3 preserves size" do
    check all mset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(mset), term()]),
        non_negative_integer()
      }) do
        result = One9.Multiset.put(mset, value, count)

        assert One9.Multiset.size(result) >= One9.Multiset.size(mset)
        assert implies count > 0,
          One9.Multiset.size(result) > One9.Multiset.size(mset)
      end
    end
  end

  property "put/3 adding zero copies doesn't corrupt struct" do
    check all mset <- t(),
              value <- one_of_([One9.Multiset.support(mset), term()]) do
      result = One9.Multiset.put(mset, value, 0)

      assert One9.Ms.strict?(result.counts)
      assert One9.Multiset.equals?(result, mset)
      check all value <- one_of_([constant(value), One9.Multiset.support(mset), term()]) do
        assert implies One9.Multiset.member?(result, value),
          One9.Multiset.member?(mset, value)
      end
    end
  end

  property "delete/3 basic correctness" do
    check all mset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(mset), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        result = One9.Multiset.delete(mset, value, count)

        case count do
          :all ->
            assert One9.Multiset.count_element(result, value) === 0

          count ->
            assert One9.Multiset.count_element(result, value) ===
              max(One9.Multiset.count_element(mset, value) - count, 0)
        end
      end
    end
  end

  property "delete/3 preserves size/1" do
    check all mset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(mset), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        assert One9.Multiset.size(One9.Multiset.delete(mset, value, count)) <=
          One9.Multiset.size(mset)
      end
    end
  end

  property "subset?/1 works as expected" do
    check all mset1 <- t(), mset2 <- t() do
      assert One9.Multiset.subset?(mset1, mset2) ===
        Enum.all?(One9.Multiset.support(mset1), fn value ->
          One9.Multiset.count_element(mset1, value) <=
            One9.Multiset.count_element(mset2, value)
        end)
    end
  end

  property "sum/2 basic correctness" do
    check all mset1 <- t(), mset2 <- t() do
      result = One9.Multiset.sum(mset1, mset2)

      check all value <- one_of_([
        One9.Multiset.support(mset1),
        One9.Multiset.support(mset2),
        term()
      ]) do
        assert One9.Multiset.count_element(result, value) ===
          One9.Multiset.count_element(mset1, value) +
            One9.Multiset.count_element(mset2, value)
      end
    end
  end

  property "sum/2 self doubles all counts" do
    check all mset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.sum(mset, mset),
        One9.Multiset.new(:maps.map(fn _, count -> count*2 end, mset.counts))
    end
  end

  property "sum/2 agrees with List._concat/1" do
    check all list1 <- list_of(term()), list2 <- list_of(term()) do
      assert One9.Multiset.equals? \
        One9.Multiset.sum(One9.Multiset.new(list1), One9.Multiset.new(list2)),
        One9.Multiset.new(list1 ++ list2)
    end
  end

  property "support/1 agrees with MapSet.new/1" do
    check all mset <- t() do
      assert Enum.sort(One9.Multiset.support(mset)) ===
        Enum.sort(MapSet.new(mset))
    end
  end

  property "union basic correctness" do
    check all mset1 <- t(), mset2 <- t() do
      result = One9.Multiset.union(mset1, mset2)

      check all element <- term() do
        assert One9.Multiset.count_element(result, element) ===
          max(
            One9.Multiset.count_element(mset1, element),
            One9.Multiset.count_element(mset2, element)
          )
      end
    end
  end

  property "union commutative" do
    check all mset1 <- t(), mset2 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.union(mset1, mset2),
        One9.Multiset.union(mset2, mset1)
    end
  end

  property "union idempotence" do
    check all mset <- t() do
      assert One9.Multiset.equals?(One9.Multiset.union(mset, mset), mset)
    end
  end
end
