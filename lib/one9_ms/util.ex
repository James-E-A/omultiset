defmodule One9.Ms.Util do
  @moduledoc false

  if not macro_exported?(Kernel, :is_pos_integer, 1) do
    defguard is_pos_integer(n) when (
      is_integer(n)
      and n > 0
    )
  end

  if not macro_exported?(Kernel, :is_non_neg_integer, 1) do
    defguard is_non_neg_integer(n) when (
      is_integer(n)
      and n >= 0
    )
  end

  if not macro_exported?(Kernel, :is_non_struct_map, 1) do
    defguard is_non_struct_map(x) when (
      is_map(x)
      and not (
        is_map_key(x, :__struct__) # minimum Elixir 1.10
        and is_atom(:erlang.map_get(:__struct__, x))
      )
    )
  end

  @spec prepend_duplicate([e], e, non_neg_integer()) :: [e] when e: term
  def prepend_duplicate(list, element, count)
  # https://github.com/erlang/otp/blob/OTP-27.2.2/lib/stdlib/src/lists.erl#L512-L513
  def prepend_duplicate(l, _, 0), do: l
  def prepend_duplicate(l, x, n), do: prepend_duplicate([x|l], x, n-1)

  @spec to_gbt(Enumerable.t({key, value})) :: :gb_trees.tree(key, value) when key: term, value: term
  def to_gbt(map) when is_non_struct_map(map) do
    :maps.fold( # minimum OTP 17.0
      &:gb_trees.insert/3,
      :gb_trees.empty(),
      map
    )
  end

  def to_gbt(enumerable) do
    Enum.reduce(
      enumerable,
      :gb_trees.empty(),
      fn {key, value}, acc -> :gb_trees.insert(key, value, acc) end
    )
  end

  @spec gbt_iter_from(
    tree :: :gb_trees.tree(key, value),
    key,
    :ordered | :reversed
  ) ::
      Enumerable.t({key, value})
    when key: term(), value: term()
  defmacro gbt_iter_from(tree, key, order \\ :ordered) do # mix cover: false positive
    quote do
      Stream.unfold(
        :gb_trees.iterator_from( # minimum OTP 27.0
          unquote(key),
          unquote(tree),
          unquote(order)
        ),
        fn acc ->
          case :gb_trees.next(acc) do
            {key, value, acc} -> {{key, value}, acc}
            :none -> nil
          end
        end
      )
    end
  end

  # https://github.com/elixir-lang/elixir/blob/v1.19.0-rc.0/lib/elixir/lib/enum.ex#L5039-L5041
  defmacro map_iter(map) do
    quote do
      Stream.unfold(
        :maps.iterator(unquote(map)), # minimum OTP 26.0
        fn acc ->
          case :maps.next(acc) do
            {key, value, acc} -> {{key, value}, acc}
            :none -> nil
          end
        end
      )
    end
  end
end
