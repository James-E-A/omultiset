#!/usr/bin/env -S elixir -S mix run

# elixir -S mix run "benchmark.exs"

Benchee.run(
  %{
    "One9.Ms.at/2" => fn
      {ms, index} -> One9.Ms.at(ms, index)
    end
  },
  print: %{fast_warning: false},
  inputs: %{
    "lax, in range" =>
      StreamData.map_of(StreamData.term(), StreamData.non_negative_integer())
      |> StreamData.bind(fn ms ->
        if (size = One9.Ms.size(ms)) > 0 do
          last_ = size - 1
          {StreamData.constant(ms), StreamData.integer(0..last_)}
        else
          StreamData.constant({ms, Range.new(0, -1, 1)})
        end
      end)
  },
  before_each: fn stream -> stream |> Enum.take(1) |> hd() end
)

Benchee.run(
  %{
    "One9.Ms.delete/3" => fn
      {ms, element, count} -> One9.Ms.delete(ms, element, count)
    end,
    "One9.Ms.delete/4 (:lax)" => fn
      {ms, element, count} -> One9.Ms.delete(ms, element, count, :lax)
    end,
  },
  print: %{fast_warning: false},
  inputs: %{
    "present elements" =>
      StreamData.map_of(StreamData.term(), StreamData.non_negative_integer(), min_length: 1)
      |> StreamData.filter(&not One9.Ms.empty?(&1))
      |> StreamData.bind(&{
        StreamData.constant(&1),
        StreamData.member_of(One9.Ms.support(&1)),
        StreamData.one_of([:all, StreamData.non_negative_integer()])
      }),
    "present elements (including 0-quantity)" =>
      StreamData.map_of(StreamData.term(), StreamData.non_negative_integer(), min_length: 1)
      |> StreamData.filter(&not One9.Ms.empty?(&1)) # ensure results are comparable to present elements
      |> StreamData.bind(&{
        StreamData.constant(&1),
        StreamData.member_of(Map.keys(&1)),
        StreamData.one_of([:all, StreamData.non_negative_integer()])
      }),
    "arbitrary elements" =>
      StreamData.map_of(StreamData.term(), StreamData.non_negative_integer(), min_length: 1)
      |> StreamData.filter(&not One9.Ms.empty?(&1)) # ensure results are comparable to present elements
      |> StreamData.bind(&{
        StreamData.constant(&1),
        StreamData.term(),
        StreamData.one_of([:all, StreamData.non_negative_integer()])
      }),
  },
  before_each: fn stream -> stream |> Enum.take(1) |> hd() end
)

Benchee.run(
  %{
    "One9.Ms.delete/3" => fn
      {ms, element, count} -> One9.Ms.delete(ms, element, count)
    end,
    "One9.Ms.delete/4 (:strict)" => fn
      {ms, element, count} -> One9.Ms.delete(ms, element, count, :strict)
    end,
  },
  print: %{fast_warning: false},
  inputs: %{
    "present elements" =>
      StreamData.map_of(StreamData.term(), StreamData.positive_integer(), min_length: 1)
      |> StreamData.bind(&{
        StreamData.constant(&1),
        StreamData.member_of(One9.Ms.support(&1)),
        StreamData.one_of([:all, StreamData.non_negative_integer()])
      }),
    "arbitrary elements" =>
      StreamData.map_of(StreamData.term(), StreamData.positive_integer(), min_length: 1)
      |> StreamData.bind(&{
        StreamData.constant(&1),
        StreamData.term(),
        StreamData.one_of([:all, StreamData.non_negative_integer()])
      }),
  },
  before_each: fn stream -> stream |> Enum.take(1) |> hd() end
)
