
defmodule TokenizerTestWeb do

  @compile {:no_warn_undefined, [Console]}
  def start() do
    text_to_tokenize = "KROWA JE TRAWE."

    {_, tokenized_word, rest, _, _, _} = result = text_to_tokenize |> :unicode.characters_to_list() |> String.Tokenizer.tokenize()
    IO.puts("FIRST TOKENIZED WORD: ")
    IO.puts(tokenized_word)
    IO.puts("REST OF SENTENCE: ")
    IO.puts(rest)
   result
  end
end
