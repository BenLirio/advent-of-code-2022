defmodule Parser do
  def parse_idx(x) do
    Regex.run(~r/^Monkey (\d+):$/, x)
    |> List.last
    |> String.to_integer
  end
  def parse_starting_items(x) do
    x
    |> String.split(":")
    |> List.last
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
  end
  def parse_operation(x) do
    [ op0, bop, op1 ] =
      Regex.run(~r/^Operation: new = (old|\d+) (\*|\+) (old|\d+)$/, x)
      |> tl
    %{ op0: op0, bop: bop, op1: op1 }
  end
  def parse_test(x) do
    Regex.run(~r/^Test: divisible by (\d+)$/, x)
    |> List.last
    |> String.to_integer
  end
  def parse_on_true(x) do
    Regex.run(~r/^If true: throw to monkey (\d+)$/, x)
    |> List.last
    |> String.to_integer
  end
  def parse_on_false(x) do
    Regex.run(~r/^If false: throw to monkey (\d+)$/, x)
    |> List.last
    |> String.to_integer
  end
  def parse_monkey(x) do
    [
      idx_str,
      starting_items_str,
      operation_str,
      test_str,
      on_true_str,
      on_false_str
    ] = x
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    %{
      idx: parse_idx(idx_str),
      starting_items: parse_starting_items(starting_items_str),
      operation: parse_operation(operation_str),
      test: parse_test(test_str),
      on_true: parse_on_true(on_true_str),
      on_false: parse_on_false(on_false_str)
    }
  end

  def parse(x) do x
    |> String.split("\n\n")
    |> Enum.map(&parse_monkey/1)
  end
end

defmodule Monkey do
  def get_op(op) do case op do
    "old" -> &(&1)
    _ -> fn _ -> String.to_integer(op) end
  end end
  def get_bop(x) do case x do
    "+" -> &(&1 + &2)
    "*" -> &(&1 * &2)
  end end
  def get_op_fn(x) do
    op0 = get_op(x.operation.op0)
    op1 = get_op(x.operation.op1)
    bop = get_bop(x.operation.bop)
    &(bop.(op0.(&1), op1.(&1)))
  end
  def get_test_fn(x) do fn v -> case rem(v, x.test) == 0 do
    true -> x.on_true
    false -> x.on_false
  end end end

  def new(x) do fn items -> fn state ->
    List.foldl(items, state, fn item, state ->
      op_fn = get_op_fn(x)
      test_fn = get_test_fn(x)
      # LCM(div0, div1, ...)
      v = rem(op_fn.(item), 9699690)
      throw_to = test_fn.(v)
      %{
        state
        | items: Map.update(
          Map.update(
            state.items,
            x.idx,
            [],
            fn items -> items -- [item] end
          ),
          throw_to,
          [v],
          fn items -> items ++ [v] end),
          monkey_counts: Map.update(
            state.monkey_counts,
            x.idx,
            0,
            fn count -> count + 1 end)
      }
    end)
  end end end
end

defmodule State do
  def new(x) do
    %{
      monkeys: List.foldl(x, %{}, fn x, acc ->
        Map.put(acc, x.idx, Monkey.new(x))
      end),
      items: List.foldl(x, %{}, fn x, acc ->
        Map.put(acc, x.idx, x.starting_items)
      end),
      monkey_counts: List.foldl(x, %{}, fn x, acc ->
        Map.put(acc, x.idx, 0)
      end)
    }
  end
  def round(state) do fn state ->
    keys = (Map.keys(state.monkeys) |> Enum.sort)
    s = keys |> Enum.min
    e = keys |> Enum.max
    List.foldl(Enum.to_list(s..e), state, fn k, state ->
      state.monkeys[k].(state.items[k]).(state)
    end)
  end end
end

defmodule Main do
  def run do
    {:ok, file_str} = File.read("input.txt")
    state = State.new(Parser.parse(file_str))
    List.foldl(
      Enum.to_list(1..10000),
      state,
      fn _, state ->
        State.round(state).(state)
      end
    )
  end
end
