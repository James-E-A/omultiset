defmodule One9.Ms do
  @type t(value) :: %{optional(value) => pos_integer()}
  @type t() :: t(term)

  @type t_lax(value) :: %{optional(value) => non_neg_integer()}
  @type t_lax() :: t_lax(term)

  @moduledoc """
  Operations on simple non-struct multiplicity maps ("multisets", `t:t/1`).

  "Lax" multisets (`t:t_lax/1`) may include some entries for absent elements, and are, by
  default, handled correctly by **all methods** in this module.

      iex> One9.Ms.counts([1, 2, 1, 3]) # like Python's "collections.Counter(...)"
      %{1 => 2, 2 => 1, 3 => 1} # 1 occurs twice, 2 occurs once, and 3 occurs once.

      iex> One9.Ms.to_list(%{1 => 2, 2 => 1, 3 => 1}) |> Enum.sort()
      [1, 1, 2, 3] # the original list is got back, order notwithstanding

      iex> One9.Ms.counts(nil: 5) # WARNING: GENERALLY NOT WHAT YOU WANT...
      ...> |> One9.Ms.to_list()
      [{nil, 5}] # the tuple {nil, 5} occurs once.
      iex> One9.Ms.from_counts(nil: 5) # ...USE THIS INSTEAD
      ...> |> One9.Ms.to_list()
      [nil, nil, nil, nil, nil] # the atom nil occurs ten times.

      iex> One9.Ms.member?(%{"damn" => 0}, "damn")
      false # "damn" occurs zero times, so it is considered not to be a member
  """

  import One9.Ms.Util
  require One9.Ms.Util

  @doc """
  Create a well-formed multiset from any (finite) Enumerable of values.

  See also `from_counts/1`.
  """
  @spec counts(Enumerable.t(e)) :: t(e) when e: term
  def counts(elements \\ [])

  def counts(%One9.Multiset{} = multiset) do
    One9.Multiset.to_counts(multiset)
  end

  def counts(%MapSet{} = set) do
    # minimum Elixir 1.14
    Map.from_keys(MapSet.to_list(set), 1)
  end

  def counts(enumerable) do
    Enum.reduce(enumerable, %{}, &Map.update(&2, &1, 1, fn n -> n+1 end))
  end

  @type t0(value) :: Enumerable.t({value, non_neg_integer()})
  @type t0() :: t0(term)

  @doc """
  A more efficient alternative to `Enum.to_list(ms) |> Enum.at(index)`.

      iex> %{41 => 100, 42 => 10**100, 43 => 100} # an extremely large multiset
      ...> |> One9.Ms.at(10**99) # pull some element out of the middle of it
      42
  """
  @spec at(t0(e), non_neg_integer()) :: e when e: term
  def at(ms, index) do
    case Enum.reduce_while(map_iter(ms), 0, fn
      {element, count}, position ->
        next_position = position + count

        if index >= next_position do
          {:cont, next_position}
        else
          {:halt, {:ok, element}}
        end
    end) do
      {:ok, element} -> element
      position when is_integer(position) -> nil
    end
  end

  @doc """
  Determine the cardinality of a multiset.

      iex> One9.Ms.counts(["duck", "duck", "goose", "duck"])
      ...> |> One9.Ms.size()
      4

  See also `count_element/2` and `empty?/1`.
  """
  @spec size(t_lax) :: non_neg_integer
  def size(ms) do
    # No need for a separate "strict" path
    Enum.sum(Map.values(ms))
  end

  @doc """
  Determine the multiplicity of an element in a multiset.

      iex> ms = One9.Ms.counts(["duck", "duck", "goose", "duck"])
      %{"duck" => 3, "goose" => 1}
      iex> ms |> One9.Ms.count_element("duck")
      3
      iex> ms |> One9.Ms.count_element("buffalo")
      0

  See also `size/1`.
  """
  @spec count_element(t_lax, term) :: non_neg_integer()
  def count_element(ms, element) do
    Map.get(ms, element, 0)
  end

  @doc """
  Delete up to the given number of copies of an element from a multiset.

  The 3-argument and 2-argument forms return a well-formed multiset whenever input is
  well-formed.

      iex> %{dog: 1, cat: 1}
      ...> |> One9.Ms.delete(:dog, 1000000)
      %{cat: 1}
  """
  @spec delete(t(e), term) :: t(e) when e: term()
  @spec delete(t_lax(e), term) :: t_lax(e) when e: term()
  @spec delete(t(e), term, non_neg_integer | :all) :: t(e) when e: term()
  @spec delete(t_lax(e), term, non_neg_integer | :all) :: t_lax(e) when e: term()
  def delete(ms, element), do: delete(ms, element, :all, :strict)
  def delete(ms, element, count) when is_integer(count) or count === :all, do: delete(ms, element, count, :strict)
  def delete(ms, element, :strict), do: delete(ms, element, :all, :strict)
  def delete(ms, element, :lax), do: delete(ms, element, :all, :lax)
  def delete(ms, element, count, strict)

  def delete(ms, element, :all, :strict), do: Map.delete(ms, element)

  def delete(ms, element, :all, :lax), do: Map.replace(ms, element, 0)

  def delete(ms, element, count, :strict) when is_pos_integer(count) do
    case ms do
      %{^element => n1} ->
        n2 = n1 - count

        if n2 > 0 do
          %{ms | element => n2}
        else
          Map.delete(ms, element)
        end

      %{} ->
        ms
    end
  end

  def delete(ms, _, 0, :strict), do: ms

  def delete(ms, element, count, :lax) when is_non_neg_integer(count) do
    case ms do
      %{^element => n1} ->
        n2 = n1 - count

        if n2 > 0 do
          %{ms | element => n2}
        else
          %{ms | element => 0}
        end

      %{} ->
        ms
    end
  end

  @doc """
  Return the first multiset, less the elements in the second.

  Calculations are done soft aka "clamping".

  The 2-argument form returns a well-formed multiset whenever the first argument is
  well-formed.

      iex> %{"dog" => 1, "cat" => 10}
      ...> |> One9.Ms.difference(%{"dog" => 3, "cat" => 3})
      %{"cat" => 7}

  See also `difference!/2`.
  """
  @spec difference(t(e), t_lax) :: t(e) when e: term()
  @spec difference(t_lax(e), t_lax) :: t_lax(e) when e: term()
  def difference(ms1, ms2, strict \\ :strict)

  def difference(ms1, ms2, :strict) do
    # minimum OTP 24.0
    :maps.filtermap(
      fn
        element, n1 ->
          case ms2 do
            %{^element => n2} ->
              n3 = n1 - n2

              if n3 > 0 do
                {true, n3}
              else
                false
              end

            %{} ->
              true
          end
      end,
      ms1
    )
  end

  def difference(ms1, ms2, :lax) do
    # minimum OTP 24.0
    :maps.filtermap(
      fn
        element, n1 ->
          case ms2 do
            %{^element => n2} ->
              n3 = n1 - n2

              if n3 > 0 do
                {true, n3}
              else
                {true, 0}
              end

            %{} ->
              true
          end
      end,
      ms1
    )
  end

  @doc """
  Return the first multiset, less the elements in the second.

  Raises `KeyError` if the second element is not a subset of the first.

  The 2-argument form returns a well-formed multiset whenever the first argument is
  well-formed.

      iex> %{"dog" => 1, "cat" => 10}
      ...> |> One9.Ms.difference!(%{"dog" => 3, "cat" => 3})
      ** (KeyError) key {"dog", 3} not found in: %{"cat" => 10, "dog" => 1}

  See also `difference/2`.
  """
  @spec difference!(t(e), t_lax(e)) :: t(e) when e: term()
  @spec difference!(t_lax(e), t_lax(e)) :: t_lax(e) when e: term()
  def difference!(ms1, ms2, strict \\ :strict)

  def difference!(ms1, ms2, :strict) do
    :maps.fold(
      fn element, n2, acc ->
        case acc do
          %{^element => n1} when n1 >= n2 ->
            n3 = n1 - n2

            if n3 > 0 do
              Map.put(acc, element, n3)
            else
              Map.delete(acc, element)
            end

          %{} ->
            raise KeyError, term: ms1, key: {element, n2}
        end
      end,
      ms1,
      ms2
    )
  end

  def difference!(ms1, ms2, :lax) do
    :maps.fold(
      fn element, n2, acc ->
        case acc do
          %{^element => n1} when n1 >= n2 ->
            Map.put(acc, element, n1 - n2)

          %{} ->
            raise KeyError, term: ms1, key: {element, n2}
        end
      end,
      ms1,
      ms2
    )
  end

  @doc """
  Determine whether the multiset is empty.

  ## Examples

      iex> One9.Ms.empty?(%{})
      true
      iex> One9.Ms.empty?(%{"pony" => 1})
      false

      iex> One9.Ms.empty?(%{"lie" => 0})
      true

  See also `size/1`.
  """
  @spec empty?(t_lax) :: boolean
  @spec empty?(t_lax, :lax) :: boolean
  @spec empty?(t, :strict) :: boolean
  def empty?(ms, strict \\ :lax)
  def empty?(ms, :strict), do: map_size(ms) === 0
  def empty?(ms, :lax), do: not Enum.any?(ms, fn {_, count} -> count > 0 end)


  @doc """
  Determine whether two multisets are equal.

  ## Examples

      iex> One9.Ms.equals?(%{"dog" => 999}, %{"cat" => 1})
      false

      iex> One9.Ms.equals?(%{"dollars" => 0}, %{"cents" => 0})
      true

  See also `size/1`.
  """
  @spec equals?(t_lax, t_lax) :: boolean
  @spec equals?(t_lax, t_lax, :lax) :: boolean
  @spec equals?(t, t, :strict) :: boolean
  def equals?(ms1, ms2, strict \\ :lax)
  def equals?(ms1, ms2, :strict), do: ms1 === ms2
  def equals?(ms1, ms2, :lax), do: equals?(normalize(ms1), normalize(ms2), :strict)

  @doc """
  Construct a well-formed multiset from any enumerable of multiplicities (`t:t0/0`).

  Duplicate entries are combined additively.

  ## Examples

      iex> One9.Ms.from_counts(true: 3, false: 99, true: 1)
      %{true: 4, false: 99}

  See also `counts/1`.
  """
  @spec from_counts(counts :: t0(e)) :: t(e) when e: term
  def from_counts(counts \\ %{})
  def from_counts(ms) when is_non_struct_map(ms), do: normalize(ms)
  def from_counts(enumerable) do
    Enum.reduce(enumerable, %{}, fn
      {element, count}, acc when is_non_neg_integer(count) ->
        if count > 0 do
          case acc do
            %{^element => n} ->
              %{acc | element => n + count}

            %{} ->
              :maps.put(element, count, acc)
          end
        else
          acc
        end

      {_, _}, _acc ->
        raise ArgumentError, "entries must all be {term, non_neg_integer}"
    end)
  end

  @doc """
  Determine whether a value is present at all in a multiset.

  ## Examples

      iex> ms = %{"bug" => 0, "cheese" => 1}
      iex> ms |> One9.Ms.member?("cheese")
      true
      iex> ms |> One9.Ms.member?("bug")
      false
      iex> ms |> One9.Ms.member?("horse")
      false

  See also `count_element/2`.
  """
  @spec member?(t_lax, term) :: boolean
  @spec member?(t, term, :strict) :: boolean
  @spec member?(t_lax, term, :lax) :: boolean
  def member?(ms, element, strict \\ :lax)
  def member?(ms, element, :strict), do: is_map_key(ms, element)
  def member?(ms, element, :lax), do: count_element(ms, element) > 0

  @doc false
  @spec normalize(t_lax(e)) :: t(e) when e: term
  def normalize(ms) do
    :maps.filter(fn # minimum OTP 18.0
      _element, count when is_non_neg_integer(count) ->
        count > 0

      _, _ ->
        raise ArgumentError, "entries must all be {term, non_neg_integer}"
    end, ms)
  end

  @doc """
  Add additional copies (by default, 1 copy) of the element into the multiset.

  The 2-argument and 3-argument forms' return value is well-formed whenever the input is.

  ## Examples

      iex> %{dog: 1}
      ...> |> One9.Ms.put(:dog, 100)
      ...> |> One9.Ms.put(:cat, 9)
      ...> |> One9.Ms.put(:unicorn, 0)
      %{dog: 101, cat: 9}

      iex> %{dog: 1}
      ...> |> One9.Ms.put(:unicorn, 0, :lax)
      %{dog: 1, unicorn: 0}
  """
  @spec put(t(e1), e2, pos_integer) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t_lax(e1), e2, pos_integer) :: t_lax(e1 | e2) when e1: term, e2: term
  def put(ms, element), do: put(ms, element, 1, :strict)
  def put(ms, element, count) when is_integer(count), do: put(ms, element, count, :strict)
  def put(ms, element, :strict), do: put(ms, element, 1, :strict)
  def put(ms, element, :lax), do: put(ms, element, 1, :lax)
  def put(ms, element, count, strict)

  def put(ms, element, count, :strict) when is_pos_integer(count) do
    Map.update(ms, element, count, &(&1 + count))
  end

  def put(ms, _, 0, :strict), do: ms

  def put(ms, element, count, :lax) when is_non_neg_integer(count) do
    Map.update(ms, element, count, &(&1 + count))
  end

  @doc """
  Determine whether the first multiset is a (non-strict) subset of the second.

  Either/both operands may be non-well-formed.

      iex> One9.Ms.subset?(%{a: 1, b: 2}, %{a: 1, b: 3, c: 5})
      true

      iex> One9.Ms.subset?(%{a: 1, b: 2}, %{a: 1, b: 2})
      true

      iex> One9.Ms.subset?(%{a: 1, b: 2}, %{a: 1, b: 1})
      false
      iex> One9.Ms.subset?(%{a: 1, b: 2}, %{a: 1})
      false
  """
  @spec subset?(t_lax(), t_lax()) :: boolean()
  def subset?(ms1, ms2) do
    Enum.all?(
      map_iter(ms1),
      fn {element, n1} ->
        n1 <=
          case ms2 do
            %{^element => n2} -> n2
            %{} -> 0
          end
      end
    )
  end

  @doc """
  Convert a multiset into a List of unique elements.

      iex> One9.Ms.counts([1, 1, 2, 42, 42, 42, 42, 42])
      ...> |> One9.Ms.support()
      ...> |> Enum.sort()
      [1, 2, 42]
  """
  @spec support(t(e)) :: [e] when e: term
  @spec support(t(e), :strict) :: [e] when e: term
  @spec support(t_lax(e), :lax) :: [e] when e: term
  def support(ms, strict \\ :lax)
  def support(ms, :strict), do: Map.keys(ms)
  def support(ms, :lax), do: support(from_counts(ms), :strict)

  @spec sum(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  @spec sum(t_lax(e1), t_lax(e2)) :: t_lax(e1 | e2) when e1: term, e2: term
  def sum(ms1, ms2) do
    Map.merge(ms1, ms2, fn _, n1, n2 -> n1 + n2 end)
  end

  @doc """
  Return the cardinality of the support of a multiset.

  See also `size/1`, to get the cardinality of the multiset itself.

  ## Examples

      iex> One9.Ms.support_count(%{a: 1, b: 99})
      2

      iex> One9.Ms.support_count(%{a: 99, b: 0})
      1

      iex> One9.Ms.support_count(%{a: 99, b: 0}, :strict) # INCORRECT
      2 # Only pass :strict when you can **guarantee** the multiset is well-formed!
  """
  @spec support_count(t_lax()) :: non_neg_integer()
  @spec support_count(t_lax(), :lax) :: non_neg_integer()
  @spec support_count(t(), :strict) :: non_neg_integer()
  def support_count(ms, strict \\ :lax)

  def support_count(ms, :strict) do
    map_size(ms)
  end

  def support_count(ms, :lax) when is_map(ms) do
    Enum.reduce(
      ms,
      0,
      fn {_, count}, acc when is_non_neg_integer(count) ->
        if count > 0, do: acc + 1, else: acc
      end
    )
  end

  @doc """
  ## Examples

      iex> One9.Ms.symmetric_difference(
      ...>   %{"dog" => 10, "frog" => 1, "rat" => 100},
      ...>   %{"cat" => 20, "frog" => 1, "rat" => 67}
      ...> )
      %{"dog" => 10, "cat" => 20, "rat" => 33}
  """
  @spec symmetric_difference(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def symmetric_difference(ms1, ms2) do
    if map_size(ms2) <= map_size(ms1) do
      # minimum OTP 17.0
      :maps.fold(
        fn
          element, n2, acc ->
            case acc do
              %{^element => n1} ->
                n3 = abs(n1 - n2)
                if n3 > 0 do
                  %{acc | element => n3}
                else
                  :maps.remove(element, acc)
                end

              %{} ->
                if n2 > 0 do
                  :maps.put(element, n2, acc)
                else
                  acc
                end
            end
        end,
        ms1,
        ms2
      )
    else
      symmetric_difference(ms2, ms1)
    end
  end

  @doc false
  @spec to_stream(t_lax(e)) :: Enumerable.t(e) when e: term
  def to_stream(ms) do
    map_iter(ms)
    |> Stream.flat_map(fn {element, count} -> Stream.duplicate(element, count) end)
  end

  @doc """
  Convert a multiset into a complete List of elements (including repeats).

  ## Examples

      iex> One9.Ms.to_list(%{a: 1, b: 2})
      ...> |> Enum.sort()
      [:a, :b, :b]

      iex> One9.Ms.to_list([a: 1, b: 2, a: 3])
      ...> |> Enum.sort()
      [:a, :a, :a, :a, :b, :b]
  """
  @spec to_list(t0(e)) :: [e] when e: term

  def to_list(ms) when is_non_struct_map(ms) do
    to_list(map_iter(ms))
  end

  def to_list(enumerable) do
    Enum.reduce(enumerable, [], fn
      {element, count}, acc ->
        prepend_duplicate(acc, element, count)
    end)
    |> :lists.reverse()
  end

  @doc false
  @spec to_tree_1(t(e)) ::
      {
        size :: non_neg_integer(),
        :gb_trees.tree(
          start_index :: non_neg_integer(),
          {e, chunk_size :: non_neg_integer()}
        )
      }
    when e: term
  def to_tree_1(ms) do
    {size, tree} =
      Enum.reduce(
        map_iter(ms),
        {0, :gb_trees.empty()},
        fn {element, count}, {running_count, tree} ->
          {
            running_count + count,
            :gb_trees.insert(running_count, {element, count}, tree)
          }
        end
      )

    {size, :gb_trees.balance(tree)}
  end

  @doc false
  @spec to_tree(t(e)) ::
      :gb_trees.tree(
        start_index :: non_neg_integer(),
        {e, chunk_size :: non_neg_integer()}
      )
    when e: term
  def to_tree(ms) do
    to_tree_1(ms) |> elem(1)
  end

  @doc """
  Return the union of two multisets.

  Either/both operands *may* be non-well-formed.
  Result will be well-formed whenever both operands are well-formed.

      iex> One9.Ms.union(%{a: 1, b: 2}, %{b: 1, c: 3})
      %{a: 1, b: 2, c: 3}
  """
  @spec union(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  @spec union(t_lax(e1), t_lax(e2)) :: t_lax(e1 | e2) when e1: term, e2: term
  def union(ms1, ms2) do
    Map.merge(ms1, ms2, fn _, n1, n2 -> max(n1, n2) end)
  end

  @doc """
  Return the intersection of two multisets.

  Either/both operands *may* be non-well-formed.
  Result will always be well-formed.

      iex> One9.Ms.intersection(%{a: 1, b: 2, c: 3}, %{b: 1, c: 2})
      %{b: 1, c: 2}

      iex> One9.Ms.intersection(%{a: 1, b: 999, z: 999}, %{a: 3, b: 2, c: 1})
      %{a: 1, b: 2}
  """
  @spec intersection(t_lax(e | e1), t_lax(e | e2)) :: t(e) when e: term, e1: term, e2: term
  def intersection(ms1, ms2) do
    if map_size(ms1) <= map_size(ms2) do
      # minimum OTP 24.0
      :maps.filtermap(
        fn
          element, n1 ->
            if n1 > 0 do
              case ms2 do
                %{^element => n2} ->
                  if n2 < n1 do
                    {true, n2}
                  else
                    true
                  end

                %{} ->
                  false
              end
            else
              false
            end
        end,
        ms1
      )
    else
      intersection(ms2, ms1)
    end
  end

  @doc """
      iex> One9.Ms.take(%{a: 1, b: 2}, :b, 1)
      {%{a: 1, b: 1}, [:b]}

      iex> One9.Ms.take(%{a: 1, b: 2}, :b, 3)
      {%{a: 1}, [:b, :b]}

      iex> One9.Ms.take(%{a: 1, b: 2}, :z, 999)
      {%{a: 1, b: 2}, []}
  """
  @spec take(t(e), e1, non_neg_integer()) :: {t(e), [e1]} when e: term, e1: term
  def take(ms, element, count) when is_pos_integer(count) do
    case ms do
      %{^element => available} ->
        n = min(count, available)
        {delete(ms, element, n), List.duplicate(element, n)}

      %{} ->
        take(ms, element, 0)
    end
  end

  def take(ms, _, 0) do
    {ms, []}
  end

  @doc false
  def well_formed?(ms) when is_map(ms) do
    Enum.all?(
      map_iter(ms),
      fn {_, count} when is_non_neg_integer(count) ->
        count > 0
      end
    )
  end
end
