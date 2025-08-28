defmodule One9.MsTest.Util do
  @moduledoc false

  # https://elixirforum.com/t/conditional-import-and-lexical-scope/44204
  require One9.Ms.Util
  import One9.Ms.Util

  alias StreamData, as: SD

  def t(value, options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        SD.map_of(value, SD.non_negative_integer())

      {true, []} ->
        SD.map_of(value, SD.positive_integer())

      {:never, []} ->
        SD.tuple({t(value), value})
        |> SD.map(fn {ms, absent_element} -> Map.put(ms, absent_element, 0) end)
    end
  end

  def t0(value) do
    SD.one_of([
      t(value, strict: false),
      SD.list_of(SD.tuple({value, SD.non_negative_integer()})),
      SD.map( # arbitrary enumerable
        SD.list_of(SD.tuple({value, SD.non_negative_integer()})),
        &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)
      ),
      SD.mapset_of(SD.tuple({value, SD.non_negative_integer()})), # arbitrary enumerable struct
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
      SD.tuple({t(value, t_options), t(value, t_options)})
      |> SD.map(fn {ms1, ms2} -> {One9.Ms.union(ms1, ms2), ms2} end)
    else
      SD.tuple({SD.nonempty(t(value, t_options)), t(value, t_options)})
      |> SD.map(fn {ms1, ms2} -> {One9.Ms.sum(ms1, ms2), ms2} end)
    end
  end

  def t_and_nonsubset(value, options \\ []) do
    SD.tuple({t(value, options), SD.nonempty(t(value, options))})
    |> SD.filter(fn {ms1, ms2} -> not One9.Ms.subset?(ms2, ms1) end)
  end

  def not_t(options \\ []) do
    case Keyword.pop(options, :strict, false) do
      {false, []} ->
        # we will prevent even non-strict sets from showing up
        SD.filter(SD.term(), fn x ->
          not is_map(x) or
            not Enum.all?(Map.values(x), &is_non_neg_integer/1)
        end)

      {true, []} ->
        # we may allow non-strict sets to show up,
        # because they are NOT strict, thus they DO fall outside of t(strict: true)
        SD.filter(SD.term(), fn x ->
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
          [SD.member_of(enum)]
        end
    end)
    |> SD.one_of()
  end

  def enumerable(value, options) do
    case Keyword.pop!(options, :finite) do
      {true, []} ->
        SD.one_of([
          SD.list_of(value),
          SD.map(SD.list_of(value), &Stream.unfold(&1, fn [x | acc] -> {x, acc}; [] -> nil end)), # arbitrary enumerable
          ##map_of(elem(value_when_2tuple, 0), elem(value_when_2tuple, 1)) |> filter(&not is_struct(&1)),
          SD.mapset_of(value), # arbitrary enumerable struct
          ##One9.MultisetTest.t(value), # arbitrary enumerable struct
        ])

      {false, []} ->
        SD.one_of([
          enumerable(value, finite: true),
          SD.constant(Stream.repeatedly(fn -> next!(value) end)),
        ])
    end
  end

  def next!(enum) do
    enum |> Enum.take(1) |> hd()
  end
end
