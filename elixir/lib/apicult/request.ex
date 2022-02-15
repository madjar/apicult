defmodule Apicult.Request do
  @spec do_request(
          make_request :: (() -> Finch.Request.t()),
          format_response :: (term() -> any())
        ) :: any
  def do_request(make_request, format_response) do
    request = make_request.()

    {:ok, %Finch.Response{status: status} = response} =
      request
      |> Finch.request(Apicult.Finch)

    cond do
      status >= 400 ->
        raise "Got error http status: #{inspect(response)}"

      # handle redirect
      status >= 300 ->
        response.headers
        |> List.keyfind!("location", 0)
        |> elem(1)

      true ->
        attrs = Jason.decode!(response.body)

        format_response.(attrs)
    end
  end
end
