defmodule One9.Multiset do
  require One9.Ms.Util

  defstruct counts: %{}

  @opaque internal(value) :: One9.Ms.t(value)
  @type t(value) :: %__MODULE__{counts: internal(value)}
  @type t :: t(term)

  @moduledoc """
  An unordered multiplicitous container type.

      iex> mset = One9.Multiset.new(["duck", "duck", "goose"])
      iex> mset |> One9.Multiset.to_counts()
      %{"duck" => 2, "goose" => 1}
      iex> mset |> One9.Multiset.to_list() |> Enum.sort()
      ["duck", "duck", "goose"]
      iex> mset |> Enum.reduce(&(&2 <> &1))
      "duckduckgoose"
      iex> mset |> One9.Multiset.member?("spanish inquisition")
      false
  """

  import One9.Ms.Util
  alias One9.Ms

  @doc """
  Create a Multiset from a MapSet, List of elements, or Map of multiplicities.

  To construct a Multiset from any other type, use `from_counts/1` or `from_elements/1` instead.

  ## Examples

      iex> One9.Multiset.new(%{"dog" => 1, "cat" => 4})
      One9.Multiset.new(["dog", "cat", "cat", "cat", "cat"])

      iex> One9.Multiset.new([0, 1, 2, 1])
      One9.Multiset.new([0, 1, 1, 2])

      iex> One9.Multiset.new(MapSet.new([0, 1, 1, 2])) # MapSet kills duplicates
      One9.Multiset.new([0, 1, 2])

      iex> One9.Multiset.new(%{__struct__: SomeOtherStructThatWeDontKnowTheSemanticsOf})
      ** (ArgumentError) explicitly call One9.Multiset.from_counts/1 or One9.Multiset.from_elements/1 instead
  """
  @spec new(list(e) | Ms.t(e) | MapSet.t(e) | t(e)) :: t(e) when e: term
  def new(arg \\ MapSet.new([]))
  def new(list) when is_list(list), do: %__MODULE__{counts: Ms.counts(list)}
  def new(%MapSet{} = set), do: %__MODULE__{counts: Ms.counts(set)}
  def new(map) when is_non_struct_map(map), do: %__MODULE__{counts: Ms.from_counts(map)}
  def new(%__MODULE__{counts: counts}), do: %__MODULE__{counts: counts}
  def new(_) do
    raise(
      ArgumentError,
      "explicitly call One9.Multiset.from_counts/1 or One9.Multiset.from_elements/1 instead"
    )
  end

  @doc """
  Create a Multiset from any (finite) Enumerable of values.
  """
  @spec from_elements(Enumerable.t(e)) :: t(e) when e: term
  def from_elements(enumerable), do: %__MODULE__{counts: Ms.counts(enumerable)}

  @doc """
  Create a Multiset from any (finite) Enumerable of `{t:term/0, t:non_neg_integer/0}` tuples.

  Duplicate entries for the same element are accepted, and will be folded in additively.
  """
  @spec from_counts(Ms.t0(e)) :: t(e) when e: term
  def from_counts(enumerable), do: %__MODULE__{counts: Ms.from_counts(enumerable)}

  @doc """
  A more efficient alternative to `Enum.to_list(ms) |> Enum.at(index)`.

  ## Examples

      iex> One9.Multiset.from_counts(%{ # an extremely large multiset
      ...>   41 => 100,
      ...>   42 => 10**100,
      ...>   43 => 100
      ...> })
      ...> |> One9.Multiset.at(10**99) # pull some element out of the middle of it
      42
  """
  def at(%__MODULE__{counts: ms}, index), do: Ms.at(ms, index)

  @doc """
  Return the cardinality of a Multiset.

  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.size()
      3

  See also `support_count/1`.
  """
  @spec size(t) :: non_neg_integer
  def size(%__MODULE__{counts: ms}), do: Ms.size(ms)

  @spec empty?(t) :: boolean
  def empty?(%__MODULE__{counts: ms}), do: Ms.empty?(ms, :strict)

  def equals?(%__MODULE__{counts: ms1}, %__MODULE__{counts: ms2}),
    do: Ms.equals?(ms1, ms2, :strict)

  @doc """
  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.member?(:a)
      true

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.member?(:z)
      false
  """
  @spec member?(t(), term()) :: boolean()
  def member?(%__MODULE__{counts: ms}, element), do: Ms.member?(ms, element, :strict)

  @doc """
  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.count_element(:b)
      2
  """
  @spec count_element(t(), term()) :: non_neg_integer()
  def count_element(%__MODULE__{counts: ms}, element), do: Ms.count_element(ms, element)

  @doc """
  Convert a Multiset into a simple Map of elements to their multiplicities.

  ## Examples

      iex> One9.Multiset.new([:a, :b, :b])
      ...> |> One9.Multiset.to_counts()
      %{a: 1, b: 2}
  """
  @spec to_counts(t(e)) :: Ms.t(e) when e: term
  def to_counts(%__MODULE__{counts: ms}), do: ms

  @doc """
  Convert a Multiset into a complete List of elements (including repeats).

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.to_list()
      ...> |> Enum.sort()
      [:a, :b, :b]
  """
  @spec to_list(t(e)) :: [e] when e: term
  def to_list(%__MODULE__{counts: ms}), do: Ms.to_list(ms)

  @doc """
  Convert a Multiset into a List of unique elements.

  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 2, c: 0})
      ...> |> One9.Multiset.support()
      ...> |> Enum.sort()
      [:a, :b]
  """
  @spec support(t(e)) :: [e] when e: term
  def support(%__MODULE__{counts: ms}), do: Ms.support(ms, :strict)

  @doc """
  Return the cardinality of the support of a Multiset.

  See also `size/1`.
  """
  @spec support_count(t()) :: non_neg_integer()
  def support_count(%__MODULE__{counts: ms}), do: Ms.support_count(ms, :strict)

  @doc false
  @spec to_tree_1(t(e)) ::
      {size :: non_neg_integer(),
        :gb_trees.tree(start_index :: non_neg_integer(), {e, chunk_size :: non_neg_integer()})}
    when e: term
  def to_tree_1(%__MODULE__{counts: ms}), do: Ms.to_tree_1(ms)

  @doc false
  @spec to_tree(t(e)) ::
      :gb_trees.tree(
        start_index :: non_neg_integer(),
        {e, chunk_size :: non_neg_integer()}
      )
    when e: term
  def to_tree(%__MODULE__{counts: ms}), do: Ms.to_tree(ms)

  defimpl Inspect, for: One9.Multiset do
    def inspect(multiset, opts) do
      size = One9.Multiset.size(multiset)

      cond do
        size === 0 ->
          "One9.Multiset.new()"

        :math.floor(:math.sqrt(size)) > One9.Multiset.support_count(multiset) ->
          Inspect.Algebra.concat([
            "One9.Multiset.new(",
            Inspect.Map.inspect(One9.Multiset.to_counts(multiset), opts),
            ")"
          ])

        true ->
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
    def count(multiset), do: {:ok, One9.Multiset.size(multiset)}

    def member?(multiset, element), do: {:ok, One9.Multiset.member?(multiset, element)}

    def reduce(multiset, acc, fun) do
      One9.Multiset.to_stream(multiset)
      |> Enumerable.reduce(acc, fun)
    end

    def slice(multiset) do
      {size, tree} = One9.Multiset.to_tree_1(multiset)

      {:ok, size, fn start, length, step ->
        stop = start + step * (length - 1)

        Enum.reduce(
          gbt_iter_from(tree, stop, :reversed),
          {stop, [], length},
          fn {cur_start, {element, _}}, {index, acc, remaining} ->
            cur_index = index - cur_start

            if cur_index >= 0 do
              n = min(div(cur_index, step) + 1, remaining)
              {index - step * n, One9.Ms.Util.prepend_duplicate(acc, element, n), remaining - n}
            else
              {index, acc, remaining}
            end
          end
        )
        |> elem(1)
      end}
    end

    # forward-looking fast path...
    # https://github.com/elixir-lang/elixir/blob/v1.19.0-rc.0/lib/elixir/lib/enum.ex#L992-L1002
    def empty?(multiset), do: {:ok, One9.Multiset.empty?(multiset)}
  end

  @doc """
  Determine whether the first Multiset is a (non-strict) subset of the second.

  ## Examples

      iex> One9.Multiset.subset?(
      ...>   One9.Multiset.new([1, 2]),
      ...>   One9.Multiset.new([1, 1, 2])
      ...> )
      true

      iex> One9.Multiset.subset?(
      ...>   One9.Multiset.new([1, 1, 2]),
      ...>   One9.Multiset.new([1, 1, 2])
      ...> )
      true

      iex> One9.Multiset.subset?(
      ...>   One9.Multiset.new([1, 2, 2]),
      ...>   One9.Multiset.new([1, 1, 2])
      ...> )
      false
  """
  @spec subset?(t(), t()) :: boolean()
  def subset?(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}), do: Ms.subset?(lhs, rhs)

  @doc """
  Return the union of two Multisets.
  """
  @spec union(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def union(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.union(lhs, rhs)}

  @doc """
  Return the intersection of two Multisets.
  """
  @spec intersection(t(e | e1), t(e | e2)) :: t(e) when e: term, e1: term, e2: term
  def intersection(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.intersection(lhs, rhs)}

  @doc """
  Return the sum of two Multisets.
  """
  @spec sum(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def sum(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.sum(lhs, rhs)}

  @doc """
  Return the first Multiset less any copies of elements the second Multiset shares.

  This difference is "soft" or "clamping". See also `difference!/1`.
  """
  @spec difference(t(e), t()) :: t(e) when e: term
  def difference(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.difference(lhs, rhs)}

  @doc """
  Return the first Multiset less any copies of elements the second Multiset shares.

  Raises if the first Multiset is not a subset of the second. See also `difference/1`.
  """
  @spec difference!(t(e), t(e)) :: t(e) when e: term
  def difference!(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.difference!(lhs, rhs)}

  @doc """
  ### Example

      iex> One9.Multiset.symmetric_difference(
      ...>   One9.Multiset.new(%{a: 10, b:  2, c: 1}),
      ...>   One9.Multiset.new(%{a:  3, b: 10, c: 1})
      ...> )
      One9.Multiset.new(%{a: 7, b: 8})
  """
  @spec symmetric_difference(t(e1), t(e2)) :: t(e1 | e2) when e1: term, e2: term
  def symmetric_difference(%__MODULE__{counts: lhs}, %__MODULE__{counts: rhs}),
    do: %__MODULE__{counts: Ms.symmetric_difference(lhs, rhs)}

  @doc """
  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 2})
      ...> |> One9.Multiset.put(:b)
      One9.Multiset.new(%{a: 1, b: 3})

      iex> One9.Multiset.new(%{a: 1, b: 10})
      ...> |> One9.Multiset.put(:b, 5)
      One9.Multiset.new(%{a: 1, b: 15})
  """
  @spec put(t(e1), e2) :: t(e1 | e2) when e1: term, e2: term
  @spec put(t(e1), e2, pos_integer()) :: t(e1 | e2) when e1: term, e2: term
  def put(%__MODULE__{counts: ms}, element, size \\ 1),
    do: %__MODULE__{counts: Ms.put(ms, element, size)}


  @doc """
  ## Examples

      iex> One9.Multiset.new(%{a: 1, b: 3})
      ...> |> One9.Multiset.delete(:b)
      One9.Multiset.new(%{a: 1, b: 2})

      iex> One9.Multiset.new(%{a: 1, b: 3})
      ...> |> One9.Multiset.delete(:b, 999)
      One9.Multiset.new(%{a: 1})
  """
  @spec delete(t(e), term) :: t(e) when e: term
  @spec delete(t(e), term, :all) :: t(e) when e: term
  @spec delete(t(e), term, non_neg_integer()) :: t(e) when e: term
  def delete(%__MODULE__{counts: ms}, element, size \\ 1),
    do: %__MODULE__{counts: Ms.delete(ms, element, size)}

  @doc """
  ## Examples

      iex> One9.Multiset.new(%{a: 2, b: 3})
      ...> |> One9.Multiset.take(:b, 1)
      {One9.Multiset.new(%{a: 2, b: 2}), [:b]}

      iex> One9.Multiset.new(%{a: 2, b: 3})
      ...> |> One9.Multiset.take(:b, 999)
      {One9.Multiset.new(%{a: 2}), [:b, :b, :b]}
  """
  @spec take(t(e), e1, non_neg_integer()) :: {t(e), [e1]} when e: term, e1: term
  def take(%__MODULE__{counts: ms}, element, size) do
    {ms, l} = Ms.take(ms, element, size)
    {%__MODULE__{counts: ms}, l}
  end

  @doc false
  @spec to_stream(t(e)) :: Enumerable.t(e) when e: term
  def to_stream(%__MODULE__{counts: ms}), do: Ms.to_stream(ms)
end
