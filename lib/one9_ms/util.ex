defmodule One9.Ms.Util do
  @moduledoc false

  # polyfill pending Elixir 1.19
  # FIXME: figure out how to omit these at compile-time when unneeded
  defguard is_pos_integer(n) when is_integer(n) and n > 0 # minimum Elixir 1.6 for defguard/1
  defguard is_non_neg_integer(n) when is_integer(n) and n >= 0 # minimum Elixir 1.6 for defguard/1

  # Trade-offs:
  # Option 1: break if someone actually wants to store the atom, `:__struct__`, in a multiset
  # Option 2: generally disallow ingesting generic Enumerable structs, except for some whitelist
  # Option 3: create a parallel set of ingestion APIs for guaranteed-simple-maps vs all-other-enumerables
  # Option 4: break if someone somehow uses an integer *as* a module name, which seems *technically* possible
  #           https://github.com/elixir-lang/elixir/blob/v1.18.2/lib/elixir/lib/kernel/utils.ex#L103
  #           https://github.com/erlang/otp/blob/OTP-27.2.2/lib/stdlib/src/ets.erl#L751
  #
  # We chose Option 4 here.
  defguard is_non_struct_map_(x) when is_map(x) and not (is_map_key(x, :__struct__) and is_atom(:erlang.map_get(:__struct__, x))) # minimum Elixir 1.10 for is_map_key/1

  @spec prepend_duplicate(non_neg_integer(), e, [e1]) :: [e | e1] when e: term, e1: term
  def prepend_duplicate(count, element, list)
  def prepend_duplicate(0, _, l), do: l
  def prepend_duplicate(n, x, l), do: prepend_duplicate(n-1, x, [x|l])

  @spec to_gbt(Enumerable.t({key, value})) :: :gb_trees.tree(key, value) when key: term, value: term
  def to_gbt(map) when is_non_struct_map_(map) do
    :maps.fold(&:gb_trees.insert/3, :gb_trees.empty(), map) # minimum OTP 17.0 for :maps.fold/3
  end

  def to_gbt(enumerable) do
    Enum.reduce(enumerable, :gb_trees.empty(), fn {key, value}, acc -> :gb_trees.insert(key, value, acc) end)
  end

  @spec gbt_iter_from(:gb_trees.tree(key, value), key, :ordered | :reversed) :: Enumerable.t({key, value}) when key: term, value: term
  def gbt_iter_from(tree, key, order \\ :ordered) do
    Stream.unfold(
      :gb_trees.iterator_from(key, tree, order), # minimum OTP 27.0
      fn acc ->
        case :gb_trees.next(acc) do
          {key, value, acc} -> {{key, value}, acc}
          :none -> nil
        end
      end
    )
  end

  @spec unfold_map(%{optional(key) => value}) :: Enumerable.t({key, value}) when key: term, value: term
  def unfold_map(map) do
    Stream.unfold(
      :maps.iterator(map), # minimum OTP 21.0
      fn acc ->
        case :maps.next(acc) do
          {key, value, acc} -> {{key, value}, acc}
          :none -> nil
        end
      end
    )
  end

  @spec unfold_map(%{optional(key) => value}, (key, value -> result)) :: Enumerable.t(result) when key: term, value: term, result: term
  def unfold_map(map, fun) do
    Stream.unfold(
      :maps.iterator(map), # minimum OTP 21.0
      fn acc ->
        case :maps.next(acc) do
          {key, value, acc} -> {fun.(key, value), acc}
          :none -> nil
        end
      end
    )
  end

  @spec unfold_map_flat(%{optional(key) => value}, (key, value -> Enumerable.t(result))) :: Enumerable.t(result) when key: term, value: term, result: term
  def unfold_map_flat(map, fun) do
    Stream.flat_map(
      unfold_map(map),
      fn {key, value} -> fun.(key, value) end
    )
  end

  @spec add_into_map(%{optional(key) => value}, key, value) :: %{key => value} when key: term, value: term
  def add_into_map(map, key, n) do
    Map.update(map, key, n, &(&1 + n))
  end
end
