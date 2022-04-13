defmodule ImplementationTest do
  use ExUnit.Case

  {:ok, {:api, _, endpoints}} = Apicult.Parser.parse_file("../implementation_tests.api")

  for {:endpoint, name, url, _result, expectation} <- endpoints do
    test name do
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
