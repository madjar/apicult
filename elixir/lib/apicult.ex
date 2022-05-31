defmodule Apicult do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  defmacro __using__(opts) when is_list(opts) do
    {file, opts} = Keyword.pop(opts, :file)
    parse_and_generate(file, opts)
  end

  defmacro __using__(file) when is_binary(file) do
    parse_and_generate(file, [])
  end

  defp parse_and_generate(file, opts) do
    {:ok, api} = Apicult.Parser.parse_file(file)
    Apicult.Generator.generate_api_bindings(api, opts)
  end
end
