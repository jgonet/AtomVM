
defmodule CompilerTest do
  # @before_compile ModuleEnvironment
  @compile {:no_warn_undefined, [Console]}
  def start() do
    eval_string = """
    a = 1
    b = 2
    :erlang.whereis(:elixir_config)
    a + b
    """

    compile_string = """
    defmodule Adder do
      def add(a, b) do
      a + b
      end
    end
    """

    text_to_tokenize = "KROWA JE TRAWE"

    {_, tokenized_word, rest, _, _, _} = text_to_tokenize |> :unicode.characters_to_list() |> String.Tokenizer.tokenize()
    :elixir.start([],[])
    :erlang.whereis(:elixir_code_server) |> :erlang.display()
    :erlang.whereis(:elixir_config) |> :erlang.display()
    :erlang.whereis(:file_server_2)  |> :erlang.display()


    IO.puts("==================")
    IO.puts(text_to_tokenize)
    IO.puts(tokenized_word)
    IO.puts("==================")
    {evaluated, env } = Code.eval_string(eval_string, [], __ENV__)
    IO.puts("EVALUATED: 2 + 1 == ")
    IO.puts(evaluated)
    IO.puts("==================")
    compiled_1 = Code.eval_string(compile_string, [], __ENV__)
    IO.puts("COMPILED")
    # Console.puts(compiled_1)
  end
end
