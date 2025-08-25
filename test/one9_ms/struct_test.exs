defmodule One9.MultisetTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest One9.Multiset

  test "inspect" do
    assert inspect(One9.Multiset.new()) === "One9.Multiset.new([])"
    assert inspect(One9.Multiset.new([1, 2, 3, 3, 3])) === "One9.Multiset.new([1, 2, 3, 3, 3])"
    assert inspect(One9.Multiset.new(%{42 => 10**100})) === "One9.Multiset.new(%{42 => #{10**100}})"
  end

  def t(value) do
    # don't bother stress-testing with lax inputs; that's done in a separate test
    One9.MsTest.t(value, strict: true)
    |> map(&One9.Multiset.new/1)
  end

  defp t_with_subset(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        tuple({t(value), t(value)})
        |> map(fn
          {multiset1, multiset2} ->
            {One9.Multiset.union(multiset1, multiset2), multiset2}
        end)

      {true, []} ->
        tuple({nonempty(t(value)), t(value)})
        |> map(fn
          {multiset1, multiset2} ->
            {One9.Multiset.sum(multiset1, multiset2), multiset2}
        end)

      {_, options} ->
        raise RuntimeError, "unsupported options: #{inspect Keyword.keys(options)}"
    end
  end

  defmacrop implies(a, b) do
    quote do: not (unquote(a) and not unquote(b))
  end

  property "new passthrough" do
    check all multiset <- t(term()) do
      assert One9.Multiset.equals?(One9.Multiset.new(multiset), multiset)
    end
  end

  property "count_element agrees with Enum.count" do
    check all multiset <- t(term()) do
      list = One9.Multiset.to_list(multiset)
      check all value <- term() do
        assert One9.Multiset.count_element(multiset, value) ===
          Enum.count(list, &(&1 === value))
      end
    end
  end

  property "difference preserves length" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      assert One9.Multiset.size(One9.Multiset.difference(multiset1, multiset2)) <=
        One9.Multiset.size(multiset1)
    end
  end

  property "difference! preserves length" do
    check all {multiset1, multiset2} <- t_with_subset(term()) do
      result = One9.Multiset.difference!(multiset1, multiset2)
      assert One9.Multiset.size(result) <= One9.Multiset.size(multiset1)
    end
  end

  property "empty works as expected" do
    check all multiset <- t(term()) do
      assert One9.Multiset.empty?(multiset) === (One9.Multiset.size(multiset) === 0)
    end
  end

  property "equals? invariant under ordering" do
    check all list <- list_of(term()) do
      assert One9.Multiset.equals?(
        One9.Multiset.new(list),
        One9.Multiset.new(Enum.shuffle(list))
      )
    end
  end

  property "Enumerable member? from_elements" do
    check all list <- list_of(term()) do
      result = One9.Multiset.from_elements(list)

      check all element <- term() do
        assert Enum.member?(result, element) === Enum.member?(list, element)
      end
    end
  end

  property "from_elements round-trip" do
    check all list <- list_of(term()) do
      result = One9.Multiset.to_list(One9.Multiset.from_elements(list))
      assert Enum.sort(result) === Enum.sort(list)
    end
  end

  @empty_range Range.new(0, -1, 1)
  defp range_within(enumerable) do
    size = Enum.count(enumerable)

    if size > 0 do
      last_ = size - 1
      integer(0..last_)
      |> bind(fn first_ ->
        tuple({constant(first_), integer(first_..last_), positive_integer()})
      end)
      |> map(fn {first, last, step} ->
          Range.new(first, last, step)
      end)
    else
      constant(@empty_range)
    end
  end

  property "to_list preserves slice" do
    check all multiset <- t(term()) do
      check all range <- range_within(multiset) do
        assert Enum.slice(multiset, range) ===
          Enum.slice(One9.Multiset.to_list(multiset), range)
      end
    end
  end

  property "intersection commutative" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      assert One9.Multiset.equals? One9.Multiset.intersection(multiset1, multiset2),
        One9.Multiset.intersection(multiset2, multiset1)
    end
  end

  property "intersection idempotence" do
    check all multiset <- t(term()) do
      assert One9.Multiset.equals? One9.Multiset.intersection(multiset, multiset),
        multiset
    end
  end

  property "intersection preserves size" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      result = One9.Multiset.intersection(multiset1, multiset2)

      assert One9.Multiset.size(result) <= One9.Multiset.size(multiset1)
      assert One9.Multiset.size(result) <= One9.Multiset.size(multiset2)
    end
  end

  property "intersection produces subsets" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      result = One9.Multiset.intersection(multiset1, multiset2)

      assert One9.Multiset.subset?(result, multiset1)
      assert One9.Multiset.subset?(result, multiset2)
    end
  end

  property "put basic correctness" do
    check all multiset <- t(term()) do
      check all value <- term(), count <- one_of([non_negative_integer(), :_default]) do
        result = case count do
          :_default -> One9.Multiset.put(multiset, value)
          count -> One9.Multiset.put(multiset, value, count)
        end

        assert One9.Multiset.count_element(result, value) ===
          One9.Multiset.count_element(multiset, value) +
            case(count, do: (:_default -> 1; count -> count))
      end
    end
  end

  property "put preserves size" do
    check all multiset <- t(term()) do
      check all value <- term(), count <- one_of([non_negative_integer(), :_default]) do
        result = case count do
          :_default -> One9.Multiset.put(multiset, value)
          count -> One9.Multiset.put(multiset, value, count)
        end

        assert One9.Multiset.size(result) >= One9.Multiset.size(multiset)
        assert implies count > 0,
          One9.Multiset.size(result) > One9.Multiset.size(multiset)
      end
    end
  end

  property "put 0 doesn't corrupt struct" do
    check all multiset <- t(term()), value <- term() do
      result = One9.Multiset.put(multiset, value, 0)

      assert One9.Ms.well_formed?(result.counts)
      assert One9.Multiset.equals? result, multiset
      assert implies One9.Multiset.member?(result, value),
        One9.Multiset.member?(multiset, value)
    end
  end

  property "delete basic correctness" do
    check all multiset <- t(term()) do
      check all value <- term(), count <- one_of([non_negative_integer(), :all, :_default]) do
        result = case count do
          :_default -> One9.Multiset.delete(multiset, value)
          count -> One9.Multiset.delete(multiset, value, count)
        end

        case count do
          :all ->
            assert One9.Multiset.count_element(result, value) === 0

          :_default ->
            assert One9.Multiset.count_element(result, value) ===
              max(One9.Multiset.count_element(multiset, value) - 1, 0)

          count ->
            assert One9.Multiset.count_element(result, value) ===
              max(One9.Multiset.count_element(multiset, value) - count, 0)
        end
      end
    end
  end

  property "delete preserves size" do
    check all multiset <- t(term()) do
      check all value <- term(), count <- one_of([non_negative_integer(), :all, :_default]) do
        result = case count do
          :_default -> One9.Multiset.delete(multiset, value)
          count -> One9.Multiset.delete(multiset, value, count)
        end

        assert One9.Multiset.size(result) <= One9.Multiset.size(multiset)
      end
    end
  end

  property "subset? works as expected" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      assert One9.Multiset.subset?(multiset1, multiset2) ===
        Enum.all?(One9.Multiset.support(multiset1), fn value ->
          One9.Multiset.count_element(multiset1, value) <=
            One9.Multiset.count_element(multiset2, value)
        end)
    end
  end

  property "sum basic correctness" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      result = One9.Multiset.sum(multiset1, multiset2)
      check all element <- term() do
        assert One9.Multiset.count_element(result, element) ===
          One9.Multiset.count_element(multiset1, element) +
            One9.Multiset.count_element(multiset2, element)
      end
    end
  end

  property "sum self doubles all counts" do
    check all multiset <- t(term()) do
      assert One9.Multiset.equals? One9.Multiset.sum(multiset, multiset),
        One9.Multiset.new(:maps.map(fn _, count -> count*2 end, multiset.counts))
    end
  end

  property "support agrees with MapSet.new" do
    check all multiset <- t(term()) do
      assert Enum.sort(One9.Multiset.support(multiset)) ===
        Enum.sort(MapSet.new(multiset))
    end
  end

  property "sum agrees with List._concat" do
    check all list1 <- list_of(term()), list2 <- list_of(term()) do
      assert One9.Multiset.equals? \
        One9.Multiset.sum(One9.Multiset.new(list1), One9.Multiset.new(list2)),
        One9.Multiset.new(list1 ++ list2)
    end
  end

  property "union basic correctness" do
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
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
    check all multiset1 <- t(term()), multiset2 <- t(term()) do
      assert One9.Multiset.equals? One9.Multiset.union(multiset1, multiset2),
        One9.Multiset.union(multiset2, multiset1)
    end
  end

  property "union idempotence" do
    check all multiset <- t(term()) do
      assert One9.Multiset.equals? One9.Multiset.union(multiset, multiset), multiset
    end
  end
end
