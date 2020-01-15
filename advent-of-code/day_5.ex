defmodule Interpretter do
  def interpret_param({mode, x}, xs) do
    case mode do
      :positional -> Enum.at(xs, x)
      :immediate  -> x
    end
  end

  # Perhaps I can model the intepretter after Forth and make it a stack-based
  # interpretter with an emphasis on debugability, introspection.
  def interpret(i, xs) do
    stack = []
    op = Enum.at(xs, i)

    # map instructions into an intermediate representation (i.e. IR) where the
    # opcodes are mapped into atoms and the arguments are mapped into references
    # or literals.

    instructions =
      %{'01' => :add,
        '02' => :multiply,
        '03' => :input,
        '04' => :output,
        '05' => :jump_if_true,
        '06' => :jump_if_false,
        '07' => :less_than,
        '08' => :equal_to,
        '99' => :return}

    case xs do
      [:add, a, b, {:positional, out} | rest] ->
        a = interpret_param(a, xs)
        b = interpret_param(b, xs)
        Interpretter.interpret(i + 3, List.insert_at(xs, out, a + b))

      [:multiply, a, b, {:positional, out} | rest] ->
        a = interpret_param(a, xs)
        b = interpret_param(b, xs)
        Interpretter.interpret(i + 3, List.insert_at(xs, out, a * b))

      [:input, a | rest] -> nil
      [:output, a | rest] -> nil
      [:jump_if_true, a, b | rest] -> nil
      [:jump_if_false, a, b | rest] -> nil
      [:less_than, a, b, out | rest] -> nil
      [:equal_to, a, b, out | rest] -> nil
      [:return | _rest] -> nil
    end
  end
end
