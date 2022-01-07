defmodule Apicult do
  @moduledoc """
  Documentation for `Apicult`.
  """

  defmacro __using__(file) do
    {:ok, api} = Apicult.Parser.parse_file(file)
    Apicult.Generator.generate_api_bindings(api)
  end
end
