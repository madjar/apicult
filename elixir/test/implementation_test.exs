defmodule ImplementationTest do
  use ExUnit.Case

  {:ok, {:api, _, endpoints}} = Apicult.Parser.parse_file("../implementation_tests.api")

  for {:endpoint, name, variables, url, _result, expectation} <- endpoints do
    variables_definition =
      for {:config, name, _type, value} <- variables do
        quote do
          unquote(Macro.var(name, nil)) = unquote(value)
        end
      end

    test name do
      unquote_splicing(variables_definition)
      request = unquote(Apicult.Generator.generate_request(url))
      request_lines = dump_request(request)

      for line <- unquote(expectation) do
        assert Enum.member?(request_lines, line)
      end
    end
  end

  defp dump_request(
         %Finch.Request{body: body, headers: headers, host: host, method: method} = req
       ) do
    # Turn a Finch request into a HTTP message, for the tests
    full_path = Finch.Request.request_path(req)

    ["#{method} #{full_path} HTTP/1.1", "Host: #{host}"] ++
      for({k, v} <- headers, do: "#{k}: #{v}") ++
      String.split(body || "", "\n")
  end
end
