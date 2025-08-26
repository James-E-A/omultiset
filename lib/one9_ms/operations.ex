defmodule One9.Ms do
  @type t(value) :: %{optional(value) => pos_integer()}
  @type t() :: t(term)

  @type t_lax(value) :: %{optional(value) => non_neg_integer()}
  @type t_lax() :: t_lax(term)

  @type t0(value) :: Enumerable.t({value, non_neg_integer()})
  @type t0() :: t0(term)

  @moduledoc """
  Operations on simple non-struct multiplicity maps (aka "multisets", `t:t/1` and `t:t_lax/1`).

  Most operations have 3 modes:

   * `One9.Ms._func(%{...}, ...)`: "Just works", suitable for most use-cases. Returns
     correct results regardless of whether arguments are `t:t/1` or `t:t_lax/1`, but
     guarantees to return `t:t/1` whenever all arguments are `t:t/1`.

   * `One9.Ms._func(%{...}, ..., :strict)`: "Fast path" when invariants can be controlled,
     such as inside a struct. Guarantees to return `t:t/1`, but invokes **UB** when any
     arguments are not `t:t/1`.

   * `One9.Ms._func(%{...}, ..., :lax)`: Guarantees to preserve all keys from the inputs,
     even at the cost of having `0`-valued entries in the result. (Testing TODO, but I
     believe these methods will also be faster than the "just works" methods.)
  """

  import One9.Ms.Util
  require One9.Ms.Util

  @doc """
  Create a well-formed multiset from any (finite) Enumerable of values.

  ## Examples

      iex> One9.Ms.counts()
      %{}

      iex> One9.Ms.counts([1, 2, 1, 1, 1])
      %{1 => 4, 2 => 1} # four (4) copies of the number 1, and one (1) copy of the number 2.

      iex> One9.Ms.counts(%{1 => 4}) # WATCH OUT: This is probably not what you want!
      %{{1, 4} => 1} # one (1) copy of the tuple {1, 4}.

  See also `from_counts/1`.
  """
  @spec counts([e]) :: t(e) when e: term
  @spec counts(Enumerable.t(e)) :: t(e) when e: term

  def counts(elements \\ [])

  def counts(list) when is_list(list) do
    # Enum.reduce calls List.reverse(), but we don't care about order
    from_list(list)
  end

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

  defp from_list(list, acc \\ %{})
  defp from_list([element | rest], acc),
    do: from_list(rest, Map.update(acc, element, 1, fn n -> n+1 end))
  defp from_list([], acc), do: acc

  @doc """
  A more efficient alternative to `One9.Ms.to_list(ms) |> Enum.at(index)`.

  See also `to_list/1`.
  """
  @spec at(t(e) | t_lax(e) | t0(e), non_neg_integer()) :: e when e: term
  def at(ms, index) do
    # no need for a separate "strict" path
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

  ## Examples

      iex> %{"cat" => 10, "dog" => 10} |> One9.Ms.size()
      20

  See also `support_size/1`, `count_element/2` and `empty?/1`.
  """
  @spec size(t() | t_lax()) :: non_neg_integer
  def size(ms) do
    # No need for a separate "strict" path
    Enum.sum(Map.values(ms))
  end

  @doc """
  Determine the multiplicity of an element in a multiset.

  ## Examples

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> One9.Ms.count_element("dog")
      10

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> One9.Ms.count_element("unicorn")
      0

  See also `size/1`.
  """
  @spec count_element(t() | t_lax(), term()) :: non_neg_integer()
  def count_element(ms, element) do
    # Tentatively, no need for a separate strict path
    # because the only fast path I can think of is
    # an early-exit for absent elements based on map_is_key,
    # which seems like it would definitely cost the slow path
    # while adding extremely dubious gains in the fast path
    Map.get(ms, element, 0)
  end

  @doc """
  Delete (up to) the given number of copies of an element from a multiset.

  ## Examples

      iex> %{"cat" => 10, "dog" => 10} |> delete("cat", 1)
      %{"dog" => 10, "cat" => 9}

      iex> %{"cat" => 10, "dog" => 10} |> delete("cat", 10)
      %{"dog" => 10}

      iex> %{"cat" => 10, "dog" => 10} |> delete("cat", 999)
      %{"dog" => 10}

      iex> %{"cat" => 10, "dog" => 10} |> delete("cat", 999, :lax)
      %{"cat" => 0, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10} |> delete("unicorn", 1)
      %{"cat" => 10, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10} |> delete("unicorn", 1, :lax)
      %{"cat" => 10, "dog" => 10}

  See also `difference/2`.
  """
  @spec delete(t(e), term()) :: t(e) when e: term()
  @spec delete(t(e), term(), :strict) :: t(e) when e: term()

  @spec delete(t_lax(e), term()) :: t_lax(e) when e: term()
  @spec delete(t_lax(e), term(), :lax) :: t_lax(e) when e: term()

  @spec delete(t(e), term(), non_neg_integer() | :all) :: t(e) when e: term()
  @spec delete(t(e), term(), non_neg_integer() | :all, :strict) :: t(e) when e: term()

  @spec delete(t_lax(e), term(), non_neg_integer() | :all) :: t_lax(e) when e: term()
  @spec delete(t_lax(e), term(), non_neg_integer() | :all, :lax) :: t_lax(e) when e: term()

  def delete(ms, element), do: Map.delete(ms, element)

  def delete(ms, element, count) when is_integer(count) or count === :all,
    do: delete(ms, element, count, :strict)
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

  ## Examples

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3})
      %{"cat" => 10, "dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "cat" => 10})
      %{"dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "cat" => 999})
      %{"dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "cat" => 10}, :lax)
      %{"cat" => 0, "dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "cat" => 999}, :lax)
      %{"cat" => 0, "dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "unicorn" => 1})
      %{"cat" => 10, "dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference(%{"dog" => 3, "unicorn" => 1}, :lax)
      %{"cat" => 10, "dog" => 7}

  See also `difference!/2`, `delete/3`.
  """
  @spec difference(t(e), t() | t_lax()) :: t(e) when e: term()
  @spec difference(t(e), t() | t_lax(), :strict) :: t(e) when e: term()

  @spec difference(t_lax(e), t_lax()) :: t_lax(e) when e: term()
  @spec difference(t_lax(e), t_lax(), :lax) :: t_lax(e) when e: term()

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
  Return the first multiset, less the elements in the second, which must be a subset.

  Raises `KeyError` if the second multiset is not a subset of the first.

  You should generally **not** use this function; instead, prefer testing `subset?/2` and
  then using `difference/2`, which (I believe; TODO: test) is slightly faster.

  ## Examples

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3})
      %{"cat" => 10, "dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "cat" => 10})
      %{"dog" => 7}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "cat" => 999})
      ** (KeyError) key {"cat", 999} not found in: %{"cat" => 10, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "cat" => 10}, :lax)
      %{"dog" => 7, "cat" => 0}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "cat" => 999}, :lax)
      ** (KeyError) key {"cat", 999} not found in: %{"cat" => 10, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "unicorn" => 1})
      ** (KeyError) key {"unicorn", 1} not found in: %{"cat" => 10, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> difference!(%{"dog" => 3, "unicorn" => 0}, :lax)
      %{"dog" => 7, "cat" => 10}

      iex> %{"cat" => 10, "dog" => 10} # WATCH OUT: lax mode doesn't change the semantics!
      ...> |> difference!(%{"dog" => 3, "unicorn" => 1}, :lax)
      ** (KeyError) key {"unicorn", 1} not found in: %{"cat" => 10, "dog" => 10}

  See also `difference/2`.
  """
  @spec difference!(t(e), t(e) | t_lax(e)) :: t(e) when e: term()
  @spec difference!(t(e), t(e) | t_lax(e), :strict) :: t(e) when e: term()

  @spec difference!(t_lax(e), t_lax(e)) :: t_lax(e) when e: term()
  @spec difference!(t_lax(e), t_lax(e), :lax) :: t_lax(e) when e: term()

  def difference!(ms1, ms2) do
    :maps.fold(
      fn
        element, n2, acc when is_pos_integer(n2) ->
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

        _, 0, acc ->
          acc
      end,
      ms1,
      ms2
    )
  end

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
          %{} when n2 === 0 ->
            acc

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

      iex> One9.Ms.empty?(%{"unicorn" => 0})
      true

  See also `size/1`, `well_formed?/1`.
  """
  @spec empty?(t() | t_lax()) :: boolean()
  @spec empty?(t(), :strict) :: boolean()

  def empty?(ms) do
    not Enum.any?(
      map_iter(ms),
      fn {_, count} when is_non_neg_integer(count) -> count > 0 end
    )
  end

  def empty?(ms, :strict), do: map_size(ms) === 0

  @doc """
  Determine whether two multisets are equal.

  ## Examples

      iex> One9.Ms.equals?(
      ...>   %{"cat" => 10, "dog" => 10},
      ...>   %{"dog" => 10, "cat" => 10}
      ...> )
      true

      iex> One9.Ms.equals?(
      ...>   %{"cat" => 10, "dog" => 10},
      ...>   %{"cat" => 10, "dog" => 10, "pony" => 1}
      ...> )
      false

      iex> One9.Ms.equals?(
      ...>   %{"cat" => 10, "dog" => 10},
      ...>   %{"cat" => 10, "dog" => 10, "unicorn" => 0}
      ...> )
      true
  """
  @spec equals?(t() | t_lax(), t() | t_lax()) :: boolean()
  @spec equals?(t(), t(), :strict) :: boolean()

  def equals?(ms1, ms2) when map_size(ms2) > map_size(ms1),
    do: equals?(ms2, ms1)

  def equals?(ms1, ms2) when is_map(ms1) and is_map(ms2) do
    # short-circuiting version of symmetric_difference.
    empty?(Enum.reduce_while(
      map_iter(ms2),
      ms1,
      fn {element, n2}, acc ->
        case acc do
          %{^element => n1} ->
            if n1 === n2 do
              # maybe equal
              {:cont, Map.delete(acc, element)}
            else
              # not equal
              {:halt, acc}
            end
        end
      end
    ))
  end

  def equals?(ms1, ms2, :strict), do: ms1 === ms2

  @doc """
  Construct a well-formed multiset from any enumerable of multiplicities (`t:t0/0`).

  Duplicate entries are combined additively.

  ## Examples

      iex> One9.Ms.from_counts(%{"cat" => 10, "dog" => 10, "unicorn" => 0})
      %{"cat" => 10, "dog" => 10}

      iex> One9.Ms.from_counts(true: 99, nil: 0, true: 1, false: 1)
      %{true: 100, false: 1}

  See also `counts/1`.
  """
  @spec from_counts(counts :: t0(e)) :: t(e) when e: term

  def from_counts(counts \\ %{})

  def from_counts(ms) when is_non_struct_map(ms), do: from_lax(ms)

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

      iex> %{"cat" => 10, "dog" => 10, "unicorn" => 0}
      ...> |> One9.Ms.member?("cat")
      true

      iex> %{"cat" => 10, "dog" => 10, "unicorn" => 0}
      ...> |> One9.Ms.member?("unicorn")
      false

  See also `count_element/2`.
  """
  @spec member?(t() | t_lax(), term()) :: boolean()
  @spec member?(t(), term(), :strict) :: boolean()

  def member?(ms, element), do: count_element(ms, element) > 0

  def member?(ms, element, :strict), do: is_map_key(ms, element)

  @doc false
  @spec from_lax(t_lax(e)) :: t(e) when e: term
  def from_lax(ms) do
    :maps.filter(fn # minimum OTP 18.0
      _element, count when is_non_neg_integer(count) ->
        count > 0

      _, _ ->
        raise ArgumentError, "entries must all be {term, non_neg_integer}"
    end, ms)
  end

  @doc """
  Add additional copies (by default, 1 copy) of the element into the multiset.

  ## Examples

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> One9.Ms.put("pony")
      %{"cat" => 10, "dog" => 10, "pony" => 1}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> One9.Ms.put("unicorn", 0)
      %{"cat" => 10, "dog" => 10}

      iex> %{"cat" => 10, "dog" => 10}
      ...> |> One9.Ms.put("unicorn", 0, :lax)
      %{"cat" => 10, "dog" => 10, "unicorn" => 0}
  """
  @spec put(t(e1), e2) :: t(e1 | e2) when e1: term(), e2: term()
  @spec put(t(e1), e2, pos_integer()) :: t(e1 | e2) when e1: term(), e2: term()
  @spec put(t(e1), term(), 0) :: t(e1) when e1: term()
  @spec put(t(e1), e2, pos_integer(), :strict) :: t(e1 | e2) when e1: term(), e2: term()
  @spec put(t(e1), term(), 0, :strict) :: t(e1) when e1: term()

  @spec put(t_lax(e1), e2) :: t_lax(e1 | e2) when e1: term(), e2: term()
  @spec put(t_lax(e1), e2, non_neg_integer()) :: t_lax(e1 | e2) when e1: term(), e2: term()
  @spec put(t_lax(e1), e2, non_neg_integer(), :lax) :: t_lax(e1 | e2) when e1: term(), e2: term()

  def put(ms, element), do: put(ms, element, 1, :strict)

  def put(ms, element, count) when is_non_neg_integer(count),
    do: put(ms, element, count, :strict)

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

  ## Examples

      iex> One9.Ms.subset?(
      ...>   %{"cat" => 10, "dog" => 9},
      ...>   %{"cat" => 10, "dog" => 10}
      ...> )
      true

      iex> One9.Ms.subset?(
      ...>   %{"cat" => 10, "dog" => 10},
      ...>   %{"cat" => 10, "dog" => 10}
      ...> )
      true

      iex> One9.Ms.subset?(
      ...>   %{"cat" => 10, "unicorn" => 0},
      ...>   %{"cat" => 10, "dog" => 10}
      ...> )
      true

      iex> One9.Ms.subset?(
      ...>   %{"cat" => 10, "pony" => 1},
      ...>   %{"cat" => 10, "dog" => 10}
      ...> )
      false
  """
  @spec subset?(t() | t_lax(), t() | t_lax()) :: boolean()

  @spec subset?(t(), t(), :strict) :: boolean()

  def subset?(ms1, ms2) do
    Enum.all?(
      map_iter(ms1),
      fn
        {element, n1} when is_pos_integer(n1) ->
          case ms2 do
            %{^element => n2} -> n1 <= n2
            %{} -> false
          end

        {_, 0} ->
          true
      end
    )
  end

  def subset?(ms1, ms2, :strict) do
    Enum.all?(
      map_iter(ms1),
      fn {element, n1} when is_pos_integer(n1) ->
        case ms2 do
          %{^element => n2} -> n1 <= n2
          %{} -> false
        end
      end
    )
  end

  @doc """
  Convert a multiset into a List of unique elements.

  ## Examples

      iex> One9.Ms.counts([1, 1, 2, 42, 42, 42, 42, 42])
      ...> |> One9.Ms.support()
      ...> |> Enum.sort()
      [1, 2, 42]

  See also `to_list/1`, `support_size/1`, `size/1`.
  """
  @spec support(t(e)) :: [e] when e: term
  @spec support(t(e), :strict) :: [e] when e: term

  @spec support(t_lax(e)) :: [e] when e: term
  @spec support(t_lax(e), :lax) :: [e] when e: term

  def support(ms), do: support(ms, :lax)

  def support(ms, :strict), do: Map.keys(ms)

  def support(ms, :lax), do: support(from_counts(ms), :strict)

  @spec sum(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term

  @spec sum(t_lax(e1), t_lax(e2)) :: t_lax(e1 | e2) when e1: term, e2: term

  def sum(ms1, ms2) do
    # No need for a separate "strict" path
    Map.merge(ms1, ms2, fn _, n1, n2 -> n1 + n2 end)
  end

  @doc """
  Return the cardinality of a multiset's support.

  ## Examples

      iex> %{"dog" => 10, "cat" => 10}
      ...> |> One9.Ms.support_size()
      2

      iex> %{"dog" => 10, "cat" => 10, "pony" => 1}
      ...> |> One9.Ms.support_size()
      3

      iex> %{"dog" => 10, "cat" => 10, "unicorn" => 0}
      ...> |> One9.Ms.support_size()
      2

  See also `support/1`, `size/1`.
  """
  @spec support_size(t() | t_lax()) :: non_neg_integer()
  @spec support_size(t(), :strict) :: non_neg_integer()

  def support_size(ms) when is_map(ms) do
    :maps.fold(
      fn
        _, count, acc when is_pos_integer(count) -> acc + 1
        _, 0, acc -> acc
      end,
      0,
      ms
    )
  end

  def support_size(ms, :strict), do: map_size(ms)

  @doc """
  ## Examples

      iex> One9.Ms.symmetric_difference(
      ...>   %{"cat" => 10, "dog" => 10},
      ...>   %{"cat" => 10, "dog" => 10}
      ...> )
      %{}

      iex> One9.Ms.symmetric_difference(
      ...>   %{"cat" => 10, "dog" => 4},
      ...>   %{"cat" => 3, "dog" => 10}
      ...> )
      %{"cat" => 7, "dog" => 6}

      iex> One9.Ms.symmetric_difference(
      ...>   %{"cat" => 10, "dog" => 10, "pony" => 1},
      ...>   %{"cat" => 10, "dog" => 15}
      ...> )
      %{"dog" => 5, "pony" => 1}

      iex> One9.Ms.symmetric_difference(
      ...>   %{"cat" => 10, "dog" => 10, "unicorn" => 0},
      ...>   %{"cat" => 10, "dog" => 10}, :lax
      ...> )
      %{"cat" => 0, "dog" => 0, "unicorn" => 0}
  """
  @spec symmetric_difference(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  @spec symmetric_difference(t(e1), t(e2), :strict) :: t(e1 | e2) when e1: term, e2: term

  @spec symmetric_difference(t_lax(e1), t_lax(e2)) :: t_lax(e1 | e2) when e1: term, e2: term
  @spec symmetric_difference(t_lax(e1), t_lax(e2), :lax) :: t_lax(e1 | e2) when e1: term, e2: term

  def symmetric_difference(ms1, ms2) when map_size(ms2) > map_size(ms1),
    do: symmetric_difference(ms2, ms1)

  def symmetric_difference(ms1, ms2) do
    :maps.fold(
      fn
        element, n2, acc when n2 > 0 ->
          case acc do
            %{^element => n1} ->
              n3 = abs(n1 - n2)
              if n3 > 0 do
                %{acc | element => abs(n1 - n2)}
              else
                Map.delete(acc, element)
              end

            %{} ->
              Map.put(acc, element, n2)
          end

        _, 0, acc ->
          acc
      end,
      ms1,
      ms2
    )
  end

  def symmetric_difference(ms1, ms2, strict) when map_size(ms2) > map_size(ms1),
    do: symmetric_difference(ms2, ms1, strict)

  def symmetric_difference(ms1, ms2, :strict) do
    :maps.fold(
      fn element, n2, acc ->
        case acc do
          %{^element => n1} ->
            n3 = abs(n1 - n2)
            if n3 > 0 do
              %{acc | element => abs(n1 - n2)}
            else
              Map.delete(acc, element)
            end

          %{} ->
            Map.put(acc, element, n2)
        end
      end,
      ms1,
      ms2
    )
  end

  def symmetric_difference(ms1, ms2, :lax) do
    :maps.fold(
      fn element, n2, acc ->
        case acc do
          %{^element => n1} ->
            %{acc | element => abs(n1 - n2)}

          %{} ->
            Map.put(acc, element, n2)
        end
      end,
      ms1,
      ms2
    )
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
    # No need for a separate "strict" path
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
