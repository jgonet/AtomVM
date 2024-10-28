
defmodule EvalTestWeb do
  # @before_compile ModuleEnvironment
  @compile {:no_warn_undefined, [Console]}
  def start() do
    eval_string = """
    a = 1
    b = 2
    a + b
    """
    eval_string_2 = ":erlang.whereis(:elixir_code_server)"
    :elixir.start([],[])

    {evaluated, _env } = Code.eval_string(eval_string, [], __ENV__)
    IO.puts("CODE:")
    IO.puts(eval_string)
    IO.puts("EVALUATED:")
    IO.puts(evaluated)

    {evaluated_2, _env } = Code.eval_string(eval_string_2, [], __ENV__)

    IO.puts("CODE:")
    IO.puts(eval_string_2)
    IO.puts("EVALUATED:")
    :erlang.display(evaluated_2)
  end
end
