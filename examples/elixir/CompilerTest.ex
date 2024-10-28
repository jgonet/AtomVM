
defmodule CompilerTest do
  # @before_compile ModuleEnvironment
  @compile {:no_warn_undefined, [Console]}
  def start() do
    compile_string = """
    defmodule Adder do
      def add(a, b) do
      a + b
      end
    end

    Adder.add(1,2)
    """
    :elixir.start([],[])
    :erlang.whereis(:elixir_code_server) |> :erlang.display()
    :erlang.whereis(:elixir_config) |> :erlang.display()
    :erlang.whereis(:file_server_2)  |> :erlang.display()
    IO.puts("CODE:")
    IO.puts(compile_string)

    {compiled_1, env} = Code.eval_string(compile_string, [], __ENV__)
    IO.puts(compiled_1)
    :erlang.display(env)
  end
end
