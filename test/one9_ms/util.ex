defmodule One9.MsTest.Util do
  @moduledoc false

  # https://elixirforum.com/t/conditional-import-and-lexical-scope/44204
  require One9.Ms.Util
  import One9.Ms.Util

  import StreamData

  def t(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        map_of(value, non_negative_integer())

      {true, []} ->
        map_of(value, positive_integer())

      {:never, []} ->
        tuple({t(value), value})
        |> map(fn {ms, absent_element} -> Map.put(ms, absent_element, 0) end)
    end
  end

  def t0(value) do
    one_of([
      t(value, strict: false),
      list_of(tuple({value, non_negative_integer()})),
      map( # arbitrary enumerable
        list_of(tuple({value, non_negative_integer()})),
        &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)
      ),
      mapset_of(tuple({value, non_negative_integer()})), # arbitrary enumerable struct
    ])
  end

  def t_and_subset(value, options \\ []) do
    {subset_strict, options} = Keyword.pop(options, :strict, false)
    t_options = case Keyword.pop(options, :t_strict, true) do
      {t_strict, options} when is_boolean(t_strict) ->
        options ++ [strict: t_strict]

      {nil, options} ->
        options
    end

    if not subset_strict do
      tuple({t(value, t_options), t(value, t_options)})
      |> map(fn {ms1, ms2} -> {One9.Ms.union(ms1, ms2), ms2} end)
    else
      tuple({nonempty(t(value, t_options)), t(value, t_options)})
      |> map(fn {ms1, ms2} -> {One9.Ms.sum(ms1, ms2), ms2} end)
    end
  end

  def t_and_nonsubset(value, options \\ []) do
    tuple({t(value, options), nonempty(t(value, options))})
    |> filter(fn {ms1, ms2} -> not One9.Ms.subset?(ms2, ms1) end)
  end

  def not_t(options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        # we will prevent even non-strict sets from showing up
        filter(term(), fn x ->
          not is_map(x) or
            not Enum.all?(Map.values(x), &is_non_neg_integer/1)
        end)

      {true, []} ->
        # we may allow non-strict sets to show up,
        # because they are NOT strict, thus they DO fall outside of t(strict: true)
        filter(term(), fn x ->
          not is_map(x) or
            not Enum.all?(Map.values(x), &is_pos_integer/1)
        end)
    end
  end

  def one_of_(datas_and_enumerables) do
    datas_and_enumerables
    |> Enum.flat_map(fn
      # https://github.com/whatyouhide/stream_data/blob/v1.2.0/lib/stream_data.ex#L197-L230
      data when is_struct(data, StreamData) or is_atom(data) or is_tuple(data) ->
        [data]

      enum ->
        if Enum.empty?(enum) do
          []
        else
          [member_of(enum)]
        end
    end)
    |> one_of()
  end

  def enumerable(value, options) do
    case Keyword.pop!(options, :finite) do
      {true, []} ->
        one_of([
          list_of(value),
          map(list_of(value), &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)), # arbitrary enumerable
          ##map_of(elem(value_when_2tuple, 0), elem(value_when_2tuple, 1)) |> filter(&not is_struct(&1)),
          mapset_of(value), # arbitrary enumerable struct
          ##One9.MultisetTest.t(value), # arbitrary enumerable struct
        ])

      {false, []} ->
        one_of([
          enumerable(value, finite: true),
          constant(Stream.repeatedly(fn -> next!(value) end)), # FIXME: is this going to make performance weird?
        ])
    end
  end

  def next!(enum) do
    enum |> Enum.take(1) |> hd()
  end
end
