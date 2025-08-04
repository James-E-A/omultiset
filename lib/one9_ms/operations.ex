defmodule One9.Ms do
  require One9.Ms.Util

  @type t(value) :: %{optional(value) => pos_integer()}
  @type t() :: t(term)

  @moduledoc """
  Operations on simple non-struct multiplicity maps ("multisets", `t:t/1`).

  "Non-well-formed" multisets include some entries for absent elements (`%{"damn" => 0}`),
  and may be accepted for certain operations for convenience.
  """

  import One9.Ms.Util

  @type t_lax(value) :: %{optional(value) => non_neg_integer()}
  @type t_lax() :: t_lax(term)

  @type t0(value) :: Enumerable.t({value, non_neg_integer()})
  @type t0() :: t0(term)

  # converting other data types to this type

  @doc """
  Create a well-formed multiset from any (finite) Enumerable of values.

  ### Usage

      iex> One9.Ms.from_elements([:a, :a, :b, :c])
      %{a: 2, b: 1, c: 1}
  """
  @spec from_elements(t0(e)) :: t(e) when e: term
  def from_elements(elements \\ [])

  def from_elements(%MapSet{} = set) do
    Map.from_keys(MapSet.to_list(set), 1) # minimum Elixir 1.14 for Map.from_keys/2
  end

  def from_elements(enumerable) do
    Enum.reduce(enumerable, %{}, &add_into_map(&2, &1, 1))
  end

  @doc """
  Create a well-formed multiset from any (finite) Enumerable of `{t:term/0, t:non_neg_integer/0}` tuples.

  Duplicate entries for the same element are accepted, and will be folded in additively.

  ### Usage

      iex> One9.Ms.from_counts([a: 2, b: 1, c: 10, c: 0, a: 1])
      %{a: 3, b: 1, c: 10}

      iex> One9.Ms.from_counts(%{a: 2, b: 1, c: 0})
      %{a: 2, b: 1}
  """
  @spec from_counts(t0(e)) :: t(e) when e: term
  def from_counts(counts \\ %{})

  def from_counts(ms) when is_non_struct_map_(ms) do
    :maps.filter(fn
      _element, count when is_non_neg_integer(count) ->
       count > 0
    end, ms)
  end

  def from_counts(enumerable) do
    Enum.reduce(enumerable, %{}, fn
      {element, count}, acc when is_non_neg_integer(count) ->
        if count > 0 do
          add_into_map(acc, element, count)
        else
          acc
        end
    end)
  end

  # converting from this type to other data types

  @spec to_enumerable(t(e)) :: Enumerable.t(e) when e: term
  def to_enumerable(ms), do: unfold_map_flat(ms, &Stream.duplicate/2)

  @doc """
  Convert a multiset into a complete List of elements (including repeats).

  Input *may* be non-well-formed.
  """
  @spec to_list(t0(e)) :: [e] when e: term

  def to_list(ms) when is_non_struct_map_(ms) do
    to_list(unfold_map(ms))
  end

  def to_list(enumerable) do
    Enum.reduce(enumerable, [], fn
      {element, count}, acc ->
        prepend_duplicate(count, element, acc)
    end)
    |> :lists.reverse()
  end

  # inspecting instances of this type

  @doc """
  Return the cardinality of a multiset.

  Input *may* be non-well-formed.

  See also `support_count/1`.
  """
  @spec count(t_lax()) :: non_neg_integer()
  def count(ms) do
    Enum.sum(Map.values(ms))
  end

  @doc """
  Determine whether a value is present at all in a multiset.

  Input *must* be well-formed.

  See also `count_element/2`.
  """
  @spec member?(t(), term()) :: boolean()
  def member?(ms, element) do
    is_map_key(ms, element)
  end

  @doc """
  Determine the multiplicity of an element in a multiset.

  Input *may* be non-well-formed.
  """
  @spec count_element(t_lax(), term()) :: non_neg_integer()
  def count_element(ms, element) do
    Map.get(ms, element, 0)
  end

  @doc """
  Convert a multiset into a List of unique elements.

  Input *must* be well-formed.
  """
  @spec support(t(e)) :: [e] when e: term
  def support(ms) do
    Map.keys(ms)
  end

  @doc """
  Return the cardinality of the support of a multiset.

  Argument must be well-formed.

  See also `count/1`.
  """
  @spec support_count(t()) :: non_neg_integer()
  def support_count(ms) do
    map_size(ms)
  end

  @spec slice(t(e)) :: {size :: non_neg_integer(), :gb_trees.tree(start_index :: non_neg_integer(), {e, chunk_size :: non_neg_integer()})} when e: term
  def _slice(ms) do
    {size, tree} = Enum.reduce(
      unfold_map(ms),
      {0, :gb_trees.empty()},
      fn {element, count}, {running_count, tree} ->
        {running_count + count, :gb_trees.insert(running_count, {element, count}, tree)}
      end
    )

    {size, :gb_trees.balance(tree)}
  end

  @spec slice(t(e)) :: :gb_trees.tree(start_index :: non_neg_integer(), {e, chunk_size :: non_neg_integer()}) when e: term
  def slice(ms) do
    _slice(ms) |> elem(1)
  end

  @spec at(t0(e), non_neg_integer()) :: e when e: term
  def at(ms, index) do
    case Enum.reduce_while(
      unfold_map(ms),
      0,
      fn {element, count}, running_count ->
        end_of_this_bin = running_count + count

        if index >= end_of_this_bin do
          {:cont, end_of_this_bin}
        else
          {:halt, {:ok, element}}
        end
      end
    ) do
      {:ok, element} -> element
      _ -> raise Enum.OutOfBoundsError
    end
  end

  # binary operations

  @doc """
  Determine whether the first multiset is a (non-strict) subset of the second.

  First operand must be well-formed.
  """
  @spec subset?(t(), t()) :: boolean()
  def subset?(ms1, ms2) do
    unfold_map(ms1, fn
      element, n1 ->
        case ms2 do
          %{^element => n2} -> 
            n1 <= n2

          %{} ->
            false
        end
    end)
    |> Enum.all?()
  end

  @doc """
  Return the union of two multisets.

  Either/both operands *may* be non-well-formed.
  Result will be well-formed whenever both operands are well-formed.
  """
  @spec union(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def union(ms1, ms2) do
    Map.merge(ms1, ms2, fn _, n1, n2 -> max(n1, n2) end)
  end

  @spec intersection(t(e | e1), t(e | e2)) :: t(e) when e: term, e1: term, e2: term
  def intersection(ms1, ms2) do
    if map_size(ms1) <= map_size(ms2) do
      :maps.filtermap(fn # minimum OTP 24.0
        element, n1 ->
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
      end, ms1)
    else
      intersection(ms2, ms1)
    end
  end

  @spec sum(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def sum(ms1, ms2) do
    Map.merge(ms1, ms2, fn _, n1, n2 -> n1 + n2 end)
  end

  @spec difference(t(e), t()) :: t(e) when e: term
  def difference(ms1, ms2) do
    :maps.filtermap(fn # minimum OTP 24.0
      element, n1 ->
        case ms2 do
          %{^element => n2} ->
            n3 = n1 - n2
            if n3 <= 0 do
              false
            else
              {true, n3}
            end

          %{} ->
            true
        end
    end, ms1)
  end

  @spec difference!(t(e), t(e)) :: t(e) when e: term
  def difference!(ms1, ms2) do
    :maps.fold(fn
      element, n2, acc ->
        case acc do
          %{^element => n1} when n1 >= n2 ->
            n3 = n1 - n2
            if n3 > 0 do
              Map.put(acc, element, n3)
            else
              Map.delete(acc, element)
            end

          %{} ->
            raise KeyError, term: ms1, key: List.duplicate(element, n2)
        end
    end, ms1, ms2)
  end

  @spec symmetric_difference(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def symmetric_difference(ms1, ms2) do
    if map_size(ms1) <= map_size(ms2) do
      :maps.fold(fn # minimum OTP 17.0
        element, n2, acc ->
          case acc do
            %{^element => n1} ->
              n3 = abs(n1 - n2)
              if n3 > 0 do
                Map.put(acc, element, n3)
              else
                Map.delete(acc, element)
              end

            %{} ->
              acc
          end
      end, ms1, ms2)
    else
      symmetric_difference(ms2, ms1)
    end
  end

  # general manipulations

  @spec put(t(e1), e2) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t(e1), e2, pos_integer()) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t(e1), e2, 0) :: t(e1) when e1: term, e2: term
  def put(ms, element, count \\ 1)

  def put(ms, element, count) when is_pos_integer(count) do
    sum(ms, %{element => count})
  end

  def put(ms, _, 0) do
    ms
  end

  @spec delete(t(e), term) :: t(e) when e: term
  @spec delete(t(e), term, :all) :: t(e) when e: term
  @spec delete(t(e), term, non_neg_integer()) :: t(e) when e: term
  def delete(ms, element, count \\ :all)

  def delete(ms, element, :all) do
    Map.delete(ms, element)
  end

  def delete(ms, element, count) when is_pos_integer(count) do
    difference(ms, %{element => count})
  end

  def delete(ms, _, 0) do
    ms
  end

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
end
