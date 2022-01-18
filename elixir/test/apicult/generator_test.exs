defmodule Apicult.GeneratorTest do
  use ExUnit.Case
  doctest Apicult.Parser

  test "Generates a proper type spec with a Client as first argument when there is a client" do
    {:ok, parsed} =
      """
      api_key
      # Call the thing
      > http http://example.com/$api_key
      """
      |> String.split("\n")
      |> Apicult.Parser.parse()

    generated = Apicult.Generator.generate_api_bindings(parsed)

    typespec = find_in_ast(generated, :@) |> List.first()

    assert Macro.to_string(typespec) ==
             Macro.to_string(
               quote do
                 @spec call_the_thing(%Client{api_key: any()}) :: any()
               end
             )
  end

  def find_in_ast({node, _, _} = ast, node), do: [ast]
  def find_in_ast({_, _, args}, node), do: find_in_ast(args, node)
  def find_in_ast(l, node) when is_list(l), do: Enum.flat_map(l, &find_in_ast(&1, node))
  def find_in_ast(_, _), do: []
end
