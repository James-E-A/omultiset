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
        |> map(fn {multiset1, multiset2} ->
          {One9.Multiset.union(multiset1, multiset2), multiset2}
        end)

      {true, []} ->
        tuple({nonempty(t(value)), t(value)})
        |> map(fn {multiset1, multiset2} ->
          {One9.Multiset.sum(multiset1, multiset2), multiset2}
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
    check all multiset <- t() do
      assert One9.Multiset.equals?(One9.Multiset.new(multiset), multiset)
    end
  end

  property "new/1 inverts to_list/1" do
    check all multiset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.new(One9.Multiset.to_list(multiset)),
        multiset
    end
  end

  property "new/1 inverts to_counts/1" do
    check all multiset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.new(One9.Multiset.to_counts(multiset)),
        multiset
    end
  end

  property "count_element/2 agrees with Enum.count/2" do
    check all multiset <- t() do
      list = One9.Multiset.to_list(multiset)
      check all value <- one_of_([One9.Multiset.support(multiset), term()]) do
        assert One9.Multiset.count_element(multiset, value) ===
          Enum.count(list, &(&1 === value))
      end
    end
  end

  property "difference/2 preserves length" do
    check all multiset1 <- t(), multiset2 <- t() do
      assert One9.Multiset.size(One9.Multiset.difference(multiset1, multiset2)) <=
        One9.Multiset.size(multiset1)
    end
  end

  property "difference!/2 preserves length" do
    check all {multiset1, multiset2} <- t_and_subset() do
      assert One9.Multiset.size(One9.Multiset.difference!(multiset1, multiset2)) <=
        One9.Multiset.size(multiset1)
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

    check all multiset <- t() do
      assert One9.Multiset.empty?(multiset) ===
        (One9.Multiset.size(multiset) === 0)
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
    check all multiset <- t(), value <- term() do
      refute One9.Multiset.equals? \
        One9.Multiset.put(multiset, value),
        multiset
    end
  end

  property "equals?/2 is preserved by put/3 with 0 quantity" do
    check all multiset <- t(), value <- term() do
      assert One9.Multiset.equals? \
        One9.Multiset.put(multiset, value, 0),
        multiset
    end
  end

  property "equals?/2 is NOT preserved by put/3 with positive quantity" do
    check all multiset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(multiset), term()]),
        positive_integer()
      }) do
        refute One9.Multiset.equals? \
          One9.Multiset.put(multiset, value, count),
          multiset
      end
    end
  end

  property "from_elements/1 preserves Enum.member?/2" do
    check all list <- list_of(term()) do
      result = One9.Multiset.from_elements(list)

      check all element <- term() do
        assert Enum.member?(result, element) === Enum.member?(list, element)
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
    check all multiset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.from_elements(One9.Multiset.to_list(multiset)),
        multiset
    end
  end

  property "to_list/1 preserves Enum.slice/2" do
    check all multiset <- t() do
      check all range <- range_within(multiset) do
        assert Enum.slice(multiset, range) ===
          Enum.slice(One9.Multiset.to_list(multiset), range)
      end
    end
  end

  property "intersection/2 is commutative" do
    check all multiset1 <- t(), multiset2 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(multiset1, multiset2),
        One9.Multiset.intersection(multiset2, multiset1)
    end
  end

  property "intersection/2 is associative" do
    check all multiset1 <- t(), multiset2 <- t(), multiset3 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(One9.Multiset.intersection(multiset1, multiset2), multiset3),
        One9.Multiset.intersection(multiset1, One9.Multiset.intersection(multiset2, multiset3))
    end
  end

  property "intersection/2 is idempotent" do
    check all multiset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.intersection(multiset, multiset),
        multiset
    end
  end

  property "intersection/2 preserves size/1" do
    check all multiset1 <- t(), multiset2 <- t() do
      result = One9.Multiset.intersection(multiset1, multiset2)

      assert One9.Multiset.size(result) <= One9.Multiset.size(multiset1)
      assert One9.Multiset.size(result) <= One9.Multiset.size(multiset2)
    end
  end

  property "intersection/2 produces subsets" do
    check all multiset1 <- t(), multiset2 <- t() do
      result = One9.Multiset.intersection(multiset1, multiset2)

      assert One9.Multiset.subset?(result, multiset1)
      assert One9.Multiset.subset?(result, multiset2)
    end
  end

  property "put/3 basic correctness" do
    check all multiset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(multiset), term()]),
        non_negative_integer()
      }) do
        assert One9.Multiset.count_element(One9.Multiset.put(multiset, value, count), value) ===
          One9.Multiset.count_element(multiset, value) + count
      end
    end
  end

  property "put/3 preserves size" do
    check all multiset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(multiset), term()]),
        non_negative_integer()
      }) do
        result = One9.Multiset.put(multiset, value, count)

        assert One9.Multiset.size(result) >= One9.Multiset.size(multiset)
        assert implies count > 0,
          One9.Multiset.size(result) > One9.Multiset.size(multiset)
      end
    end
  end

  property "put/3 adding zero copies doesn't corrupt struct" do
    check all multiset <- t(),
              value <- one_of_([One9.Multiset.support(multiset), term()]) do
      result = One9.Multiset.put(multiset, value, 0)

      assert One9.Ms.strict?(result.counts)
      assert One9.Multiset.equals?(result, multiset)
      check all value <- one_of_([constant(value), One9.Multiset.support(multiset), term()]) do
        assert implies One9.Multiset.member?(result, value),
          One9.Multiset.member?(multiset, value)
      end
    end
  end

  property "delete/3 basic correctness" do
    check all multiset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(multiset), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        result = One9.Multiset.delete(multiset, value, count)

        case count do
          :all ->
            assert One9.Multiset.count_element(result, value) === 0

          count ->
            assert One9.Multiset.count_element(result, value) ===
              max(One9.Multiset.count_element(multiset, value) - count, 0)
        end
      end
    end
  end

  property "delete/3 preserves size/1" do
    check all multiset <- t() do
      check all {value, count} <- tuple({
        one_of_([One9.Multiset.support(multiset), term()]),
        one_of([:all, non_negative_integer()])
      }) do
        assert One9.Multiset.size(One9.Multiset.delete(multiset, value, count)) <=
          One9.Multiset.size(multiset)
      end
    end
  end

  property "subset?/1 works as expected" do
    check all multiset1 <- t(), multiset2 <- t() do
      assert One9.Multiset.subset?(multiset1, multiset2) ===
        Enum.all?(One9.Multiset.support(multiset1), fn value ->
          One9.Multiset.count_element(multiset1, value) <=
            One9.Multiset.count_element(multiset2, value)
        end)
    end
  end

  property "sum/2 basic correctness" do
    check all multiset1 <- t(), multiset2 <- t() do
      result = One9.Multiset.sum(multiset1, multiset2)

      check all value <- one_of_([
        One9.Multiset.support(multiset1),
        One9.Multiset.support(multiset2),
        term()
      ]) do
        assert One9.Multiset.count_element(result, value) ===
          One9.Multiset.count_element(multiset1, value) +
            One9.Multiset.count_element(multiset2, value)
      end
    end
  end

  property "sum/2 self doubles all counts" do
    check all multiset <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.sum(multiset, multiset),
        One9.Multiset.new(:maps.map(fn _, count -> count*2 end, multiset.counts))
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
    check all multiset <- t() do
      assert Enum.sort(One9.Multiset.support(multiset)) ===
        Enum.sort(MapSet.new(multiset))
    end
  end

  property "union basic correctness" do
    check all multiset1 <- t(), multiset2 <- t() do
      result = One9.Multiset.union(multiset1, multiset2)

      check all element <- term() do
        assert One9.Multiset.count_element(result, element) ===
          max(
            One9.Multiset.count_element(multiset1, element),
            One9.Multiset.count_element(multiset2, element)
          )
      end
    end
  end

  property "union commutative" do
    check all multiset1 <- t(), multiset2 <- t() do
      assert One9.Multiset.equals? \
        One9.Multiset.union(multiset1, multiset2),
        One9.Multiset.union(multiset2, multiset1)
    end
  end

  property "union idempotence" do
    check all multiset <- t() do
      assert One9.Multiset.equals?(One9.Multiset.union(multiset, multiset), multiset)
    end
  end
end
