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

  test "Generates a function with default arguments for variables" do
    {:ok, parsed} =
      """
      # Call the thing
      api_key=pouet
      > http http://example.com/$api_key
      """
      |> String.split("\n")
      |> Apicult.Parser.parse()

    generated = Apicult.Generator.generate_api_bindings(parsed)

    {:call_the_thing, _context, args} =
      find_in_ast(generated, :call_the_thing)
      # We take the second, because the first one is the typespec
      |> Enum.at(1)

    assert Macro.to_string(args) ==
             Macro.to_string(
               quote do
                 [api_key \\ "pouet"]
               end
             )
  end


  test "Generates a client function that looks into the Config" do
    {:ok, parsed} =
      """
      api_key
      # Call the thing
      > http http://example.com/$api_key
      """
      |> String.split("\n")
      |> Apicult.Parser.parse()

    generated = Apicult.Generator.generate_api_bindings(parsed, config_app: :pouet)

    defclient =
      find_in_ast(generated, :def)
      |> Enum.at(0)

    assert Macro.to_string(defclient) ==
             Macro.to_string(
               quote do
                def client(opts \\ []) do
                  config = Application.get_env(:pouet, __MODULE__, [])
                  struct!(Client, Keyword.merge(config, opts))
                end
               end
             )
    end

  # TODO use Macro.prewalker
  def find_in_ast({node, _, _} = ast, node), do: [ast]
  def find_in_ast({_, _, args}, node), do: find_in_ast(args, node)
  def find_in_ast(l, node) when is_list(l), do: Enum.flat_map(l, &find_in_ast(&1, node))
  def find_in_ast(_, _), do: []
end
