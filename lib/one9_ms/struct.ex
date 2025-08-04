defmodule One9.Multiset do
  require One9.Ms.Util

  defstruct [counts: %{}]

  @opaque internal(value) :: One9.Ms.t(value)
  @type t(value) :: %__MODULE__{counts: internal(value)}
  @type t :: t(term)

  @moduledoc """
  An unordered multiplicitous container type.
  """

  import One9.Ms.Util
  alias One9.Ms

  # converting other data types to this type

  @doc """
  Create a Multiset from a MapSet, List of elements, or Map of multiplicities.

  To construct a Multiset from any other type, use `from_counts/1` or `from_elements/1` instead.

  ### Usage

      iex> One9.Multiset.new(MapSet.new([0, 1, 1, 2, 3, 5, 8]))
      One9.Multiset.new([0, 1, 2, 3, 5, 8])

      iex> One9.Multiset.new([0, 1, 1, 2, 3, 5, 8])
      One9.Multiset.new([0, 1, 1, 2, 3, 5, 8])

      iex> One9.Multiset.new(%{a: 2, b: 1, c: 1})
      One9.Multiset.new([:a, :a, :b, :c])

      iex> One9.Multiset.new(%{a: 2, b: 1, c: 10})
      One9.Multiset.new([:a, :a, :b, :c, :c, :c, :c, :c, :c, :c, :c, :c, :c])

      iex> One9.Multiset.new(%{a: 2, b: 1, c: 10**100})
      # alternate Inspect representation kicks in approximately when
      # the cardinality of a Multiset exceeds
      # the square of the cardinality of its support
      One9.Multiset.from_counts(%{a: 2, b: 1, c: 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000})
  """
  @spec new(list(e) | Ms.t0(e) | MapSet.t(e)) :: t(e) when e: term
  def new(arg \\ MapSet.new([]))
  def new(%MapSet{} = set), do: %__MODULE__{counts: Ms.from_elements(set)}
  def new(list) when is_list(list), do: %__MODULE__{counts: Ms.from_elements(list)}
  def new(map) when is_non_struct_map_(map), do: %__MODULE__{counts: Ms.from_counts(map)}
  def new(_), do: (raise ArgumentError, "explicitly call One9.Multiset.from_counts/1 or One9.Multiset.from_elements/1 instead")

  @doc """
  Create a Multiset from any (finite) Enumerable of values.

  ### Usage

      iex> One9.Multiset.from_elements(MapSet.new([0, 1, 1, 2, 3, 5, 8]))
      One9.Multiset.new([0, 1, 2, 3, 5, 8])

      iex> One9.Multiset.from_elements([0, 1, 1, 2, 3, 5, 8])
      One9.Multiset.new([0, 1, 1, 2, 3, 5, 8])
  """
  @spec from_elements(Enumerable.t(e)) :: t(e) when e: term
  def from_elements(enumerable), do: %__MODULE__{counts: Ms.from_elements(enumerable)}

  @doc """
  Create a Multiset from any (finite) Enumerable of `{t:term/0, t:non_neg_integer/0}` tuples.

  Duplicate entries for the same element are accepted, and will be folded in additively.

  ### Usage

      iex> One9.Multiset.from_counts(%{a: 2, b: 1, c: 10})
      One9.Multiset.new([:a, :a, :b, :c, :c, :c, :c, :c, :c, :c, :c, :c, :c])

      iex> One9.Multiset.from_counts([a: 2, b: 1, c: 0, a: 1])
      One9.Multiset.new([:a, :a, :a, :b])
  """
  @spec from_counts(Ms.t0(e)) :: t(e) when e: term
  def from_counts(enumerable), do: %__MODULE__{counts: Ms.from_counts(enumerable)}

  # inspecting instances of this type

  @spec to_enumerable(t(e)) :: Enumerable.t(e) when e: term
  def to_enumerable(%__MODULE__{counts: ms}), do: Ms.to_enumerable(ms)

  @doc """
  Return the cardinality of a Multiset.

  See also `support_count/1`.
  """
  @spec count(t()) :: non_neg_integer()
  def count(%__MODULE__{counts: ms}), do: Ms.count(ms)

  @spec member?(t(), term()) :: boolean()
  def member?(%__MODULE__{counts: ms}, element), do: Ms.member?(ms, element)

  @spec count_element(t(), term()) :: non_neg_integer()
  def count_element(%__MODULE__{counts: ms}, element), do: Ms.count_element(ms, element)

  @doc """
  Convert a Multiset into a simple Map of elements to their multiplicities.
  """
  @spec to_counts(t(e)) :: Ms.t(e) when e: term
  def to_counts(%__MODULE__{counts: ms}), do: ms

  @doc """
  Convert a Multiset into a complete List of elements (including repeats).
  """
  @spec to_list(t(e)) :: [e] when e: term
  def to_list(%__MODULE__{counts: ms}), do: Ms.to_list(ms)

  @doc """
  Convert a Multiset into a List of unique elements.
  """
  @spec support(t(e)) :: [e] when e: term
  def support(%__MODULE__{counts: ms}), do: Ms.support(ms)

  @doc """
  Return the cardinality of the support of a Multiset.

  See also `count/1`.
  """
  @spec support_count(t()) :: non_neg_integer()
  def support_count(%__MODULE__{counts: ms}), do: Ms.support_count(ms)

  def _slice(%__MODULE__{counts: ms}), do: Ms._slice(ms)

  def slice(%__MODULE__{counts: ms}), do: Ms.slice(ms)

  defimpl Inspect, for: One9.Multiset do
    def inspect(multiset, opts) do
      if :math.floor(:math.sqrt(One9.Multiset.count(multiset))) > One9.Multiset.support_count(multiset) do
        # Multiset.from_counts(%{42 => 10**100})
        Inspect.Algebra.concat([
          "One9.Multiset.from_counts(",
          Inspect.Map.inspect(One9.Multiset.to_counts(multiset), opts),
          ")"
        ])

      else
        # https://github.com/elixir-lang/elixir/blob/v1.18.2/lib/elixir/lib/map_set.ex#L444
        opts = %Inspect.Opts{opts | charlists: :as_lists}
        Inspect.Algebra.concat([
          "One9.Multiset.new(",
          Inspect.List.inspect(One9.Multiset.to_list(multiset), opts),
          ")"
        ])
      end
    end
  end

  defimpl Enumerable, for: One9.Multiset do
    def count(multiset), do: {:ok, One9.Multiset.count(multiset)}
    def member?(multiset, element), do: {:ok, One9.Multiset.member?(multiset, element)}
    def reduce(multiset, acc, fun), do: Enumerable.reduce(One9.Multiset.to_enumerable(multiset), acc, fun)
    def slice(multiset) do
      {size, tree} = One9.Multiset._slice(multiset)

      {:ok, size, fn start, length, step ->
        end_ = length*step + start - 1

        Enum.reduce_while(
          gbt_iter_from(tree, end_, :reversed),
          {nil, length, []},
          fn {position, {element, element_count}}, {offset, remaining, acc} ->
            available_element_count = case offset do
              prior_overstep when is_integer(prior_overstep) ->
                element_count - prior_overstep

              nil ->
                end_ - position
            end

            n =
              div(available_element_count + (step - 1), step)
              |> min(0)
              |> max(remaining)

            {acc, remaining} = {prepend_duplicate(n, element, acc), remaining - n}

            if remaining > 0 do
              overstep = n*step - available_element_count
              {:cont, {overstep, remaining, acc}}
            else
              {:halt, acc}
            end
          end
        )
      end}
    end
  end

  # binary operations

  @doc """
  Determine whether the first Multiset is a (non-strict) subset of the second.
  """
  @spec subset?(t(), t()) :: boolean()
  def subset?(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: Ms.subset?(lhs, rhs)

  @doc """
  Return the union of two Multisets.
  """
  @spec union(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def union(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.union(lhs, rhs)}

  @doc """
  Return the intersection of two Multisets.
  """
  @spec intersection(t(e | e1), t(e | e2)) :: t(e) when e: term, e1: term, e2: term
  def intersection(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.intersection(lhs, rhs)}

  @doc """
  Return the sum of two Multisets.
  """
  @spec sum(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def sum(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.sum(lhs, rhs)}

  @doc """
  Return the first Multiset less any copies of elements the second Multiset shares.

  This difference is "soft" or "clamping". See also `difference!/1`.
  """
  @spec difference(t(e), t()) :: t(e) when e: term
  def difference(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.difference(lhs, rhs)}

  @doc """
  Return the first Multiset less any copies of elements the second Multiset shares.

  Raises if the first Multiset is not a subset of the second. See also `difference/1`.
  """
  @spec difference!(t(e), t(e)) :: t(e) when e: term
  def difference!(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.difference!(lhs, rhs)}

  @doc """
  ### Example

      iex> One9.Multiset.symmetric_difference(
      ...>   One9.Multiset.new(%{a: 10, b:  2, c: 1}),
      ...>   One9.Multiset.new(%{a:  3, b: 10, c: 1})
      ...> )
      One9.Multiset.from_counts(%{a: 7, b: 8})
  """
  @spec symmetric_difference(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def symmetric_difference(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: %__MODULE__{counts: Ms.symmetric_difference(lhs, rhs)}

  # general manipulations

  @spec put(t(e1), e2) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t(e1), e2, pos_integer()) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t(e1), e2, 0) :: t(e1) when e1: term, e2: term
  def put(%__MODULE__{counts: ms}, element, count \\ 1), do: %__MODULE__{counts: Ms.put(ms, element, count)}

  @spec delete(t(e), term) :: t(e) when e: term
  @spec delete(t(e), term, :all) :: t(e) when e: term
  @spec delete(t(e), term, non_neg_integer()) :: t(e) when e: term
  def delete(%__MODULE__{counts: ms}, element, count \\ 1), do: %__MODULE__{counts: Ms.delete(ms, element, count)}

  @spec take(t(e), e1, non_neg_integer()) :: {t(e), [e1]} when e: term, e1: term
  def take(%__MODULE__{counts: ms}, element, count) do
    {ms, l} = Ms.take(ms, element, count)
    {%__MODULE__{counts: ms}, l}
  end
end
