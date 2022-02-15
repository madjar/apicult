defmodule Apicult.Request do
  @type request_error :: Finch.Response.t() | Exception.t() | Jason.DecodeError.t()

  @spec do_request(
          make_request :: (() -> Finch.Request.t()),
          format_response :: (term() -> any())
        ) ::
          {:ok, any}
          | {:error, request_error()}
  def do_request(make_request, format_response) do
    request = make_request.()

    with {:ok, %Finch.Response{} = response} <-
           Finch.request(request, Apicult.Finch) |> make_non_200_error,
         {:ok, attrs} <- Jason.decode(response.body) do
      {:ok, format_response.(attrs)}
    end
  end

  defp make_non_200_error({:ok, %Finch.Response{status: status} = response}) when status >= 300,
    do: {:error, response}

  defp make_non_200_error(other), do: other
end
