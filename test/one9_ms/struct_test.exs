defmodule One9.MultisetTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest One9.Multiset

  test "inspect" do
    assert inspect(One9.Multiset.new()) === "One9.Multiset.new()"
    assert inspect(One9.Multiset.new([1, 2, 3, 3, 3])) === "One9.Multiset.new([1, 2, 3, 3, 3])"
    assert inspect(One9.Multiset.new(%{42 => 10**100})) === "One9.Multiset.new(%{42 => #{10**100}})"
  end

  def t(value) do
    # don't bother stress-testing with lax inputs; that's done in a separate test
    one_of([
      list_of(value),
      One9.MsTest.t(value, strict: true),
    ])
    |> map(&One9.Multiset.new/1)
  end

  #doc "Generates elements from `other`, and also from finite `enumerable` if possible."
  defp and_members_of(extra, enumerable) do
    if not Enum.empty?(enumerable) do
      StreamData.one_of([StreamData.member_of(enumerable), extra])
    else
      extra
    end
  end

  defp has_uniqness?(enumerable, n \\ 2) do
    Enum.reduce_while(
      enumerable,
      MapSet.new(),
      &if(MapSet.size(&2) >= n, do: {:halt, &2}, else: {:cont, MapSet.put(&2, &1)})
    )
    |> MapSet.size()
    |> Kernel.>=(n)
  end

  property "count_element agrees with Enum.count" do
    check all \
      multiset <- t(term()),
      elements = One9.Multiset.to_stream(multiset),
      element <- term() |> and_members_of(multiset)
    do
      assert One9.Multiset.count_element(multiset, element) === \
             Enum.count(elements, &(&1 === element))
    end

    check all \
      elements <- list_of(term()),
      multiset = One9.Multiset.from_elements(elements),
      element <- term() |> and_members_of(elements)
    do
      assert One9.Multiset.count_element(multiset, element) === \
             Enum.count(elements, &(&1 === element))
    end
  end

  property "difference preserves length" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term()),
      multiset3 = One9.Multiset.difference(multiset1, multiset2)
    do
      assert One9.Multiset.size(multiset3) <= One9.Multiset.size(multiset1)
    end
  end

  property "difference! preserves length" do
    check all \
      multiset2 <- t(term()), # subset of multiset1
      multiset1 <- map(t(term()), &One9.Multiset.sum(multiset2, &1)),
      multiset3 = One9.Multiset.difference!(multiset1, multiset2)
    do
      assert One9.Multiset.size(multiset3) <= One9.Multiset.size(multiset1)
    end
  end

  property "empty works as expected" do
    check all multiset <- t(term()) do
      assert \
        One9.Multiset.empty?(multiset) ===
          (One9.Multiset.size(multiset) === 0)
    end
  end

  property "equals? invariant under ordering" do
    check all \
      list1 <- list_of(term(), min_length: 2) |> filter(&has_uniqness?/1),
      multiset1 = One9.Multiset.new(list1),
      list2 <- shuffle(list1),
      list1 !== list2,
      multiset2 = One9.Multiset.new(list2)
    do
      assert list1 !== list2
      assert One9.Multiset.equals?(multiset1, multiset2)
    end
  end

  property "Enumerable member? from_elements" do
    check all \
      elements <- list_of(term()),
      multiset = One9.Multiset.from_elements(elements),
      element <- term() |> and_members_of(elements)
    do
      assert \
        Enum.member?(multiset, element) ===
          Enum.member?(elements, element)
    end
  end

  property "from_elements round-trip" do
    check all \
      elements <- list_of(term()),
      multiset = One9.Multiset.from_elements(elements)
    do
      assert \
        Enum.sort(One9.Multiset.to_list(multiset)) ===
          Enum.sort(elements)

      assert \
        Enum.sort(Enum.to_list(multiset)) ===
          Enum.sort(elements)
    end
  end

  property "to_list preserves slice" do
    check all \
      multiset <- t(term()),
      elements = One9.Multiset.to_list(multiset),
      size = length(elements),
      start_index <- member_of(0..size),
      amount <- member_of(0..(size - start_index))
    do
      assert Enum.slice(multiset, start_index, amount) === \
             Enum.slice(elements, start_index, amount)
    end
  end

  property "intersection preserves size" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term()),
      multiset3 = One9.Multiset.intersection(multiset1, multiset2)
    do
      assert One9.Multiset.size(multiset3) <= min(
        One9.Multiset.size(multiset1),
        One9.Multiset.size(multiset2)
      )
    end
  end

  property "intersection produces subsets" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term()),
      multiset3 = One9.Multiset.intersection(multiset1, multiset2)
    do
      assert One9.Multiset.subset?(multiset3, multiset1)
      assert One9.Multiset.subset?(multiset3, multiset2)
    end
  end

  property "put basic correctness" do
    check all \
      multiset1 <- t(term()),
      element <- term() |> and_members_of(multiset1),
      count <- non_negative_integer(),
      multiset2 = One9.Multiset.put(multiset1, element, count)
    do
      assert One9.Multiset.count_element(multiset2, element) ===
             One9.Multiset.count_element(multiset1, element) + count
    end
  end

  property "put preserves size" do
    check all \
      multiset1 <- t(term()),
      element <- term() |> and_members_of(multiset1),
      count <- non_negative_integer(),
      multiset2 = One9.Multiset.put(multiset1, element, count)
    do
      One9.Multiset.size(multiset2) >= One9.Multiset.size(multiset1)
    end
  end

  property "delete basic correctness" do
    check all \
      multiset1 <- t(term()),
      element <- term() |> and_members_of(multiset1),
      count <- one_of([non_negative_integer(), constant(:all), constant(nil)]),
      multiset2 = case(count, do: (nil -> One9.Multiset.delete(multiset1, element); count -> One9.Multiset.delete(multiset1, element, count)))
    do
      case count do
        nil ->
          assert \
            One9.Multiset.count_element(multiset2, element) ===
              max(One9.Multiset.count_element(multiset1, element) - 1, 0)

        count when is_integer(count) ->
          assert \
            One9.Multiset.count_element(multiset2, element) ===
              max(One9.Multiset.count_element(multiset1, element) - count, 0)

        :all ->
          assert \
            One9.Multiset.count_element(multiset2, element) === 0
      end
    end
  end

  property "delete preserves size" do
    check all \
      multiset1 <- t(term()),
      element <- term() |> and_members_of(multiset1),
      count <- one_of([non_negative_integer(), :all, nil]),
      multiset2 = case(count, do: (nil -> One9.Multiset.delete(multiset1, element); count -> One9.Multiset.delete(multiset1, element, count)))
    do
      assert One9.Multiset.size(multiset2) <= One9.Multiset.size(multiset1)
    end
  end

  property "subset? works as expected" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term())
    do
      assert \
        One9.Multiset.subset?(multiset1, multiset2) ===
          not Enum.any?(
            One9.Multiset.support(multiset1),
            &(
              One9.Multiset.count_element(multiset1, &1) >
                One9.Multiset.count_element(multiset2, &1)
            )
          )
    end
  end

  property "sum works as expected" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term()),
      multiset3 = One9.Multiset.sum(multiset1, multiset2),
      element <- Enum.reduce([multiset1, multiset2, multiset3], term(), &and_members_of(&2, &1))
    do
      assert \
        One9.Multiset.count_element(multiset3, element) ===
          (
            One9.Multiset.count_element(multiset1, element) +
            One9.Multiset.count_element(multiset2, element)
          )
    end
  end

  property "support agrees with MapSet.new" do
    check all \
      multiset <- t(term()),
      support = MapSet.new(multiset)
    do
      assert Enum.sort(One9.Multiset.support(multiset)) === Enum.sort(support)
    end
  end

  property "sum agrees with List._concat" do
    check all \
      list1 <- list_of(term()),
      multiset1 = One9.Multiset.new(list1),
      list2 <- list_of(term()),
      list3 = list1 ++ list2,
      multiset2 = One9.Multiset.new(list2),
      multiset3 = One9.Multiset.new(list3)
    do
      assert One9.Multiset.equals?(multiset3, One9.Multiset.sum(multiset1, multiset2))
    end
  end

  property "union works as expected" do
    check all \
      multiset1 <- t(term()),
      multiset2 <- t(term()),
      multiset3 = One9.Multiset.union(multiset1, multiset2),
      element <- Enum.reduce([multiset1, multiset2, multiset3], term(), &and_members_of(&2, &1))
    do
      assert \
        One9.Multiset.count_element(multiset3, element) ===
          max(
            One9.Multiset.count_element(multiset1, element),
            One9.Multiset.count_element(multiset2, element)
          )
    end
  end
end
