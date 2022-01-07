defmodule Apicult do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  defmacro __using__(file) do
    {:ok, api} = Apicult.Parser.parse_file(file)
    Apicult.Generator.generate_api_bindings(api)
  end
end
