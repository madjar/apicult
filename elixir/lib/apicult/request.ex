defmodule Apicult.Request do
  @type request_error :: NonSuccessResponseError.t() | Exception.t() | Jason.DecodeError.t()

  defmodule NonSuccessResponseError do
    @type t :: %__MODULE__{request: Finch.Request.t(), response: Finch.Response.t()}
    defexception [:request, :response]

    @impl true
    def message(%__MODULE__{request: request, response: response}) do
      "Non-success response (#{response.status}) for #{request.method} #{request.host}#{request.path}"
    end
  end

  @spec run_request(
          make_request :: (() -> Finch.Request.t()),
          format_response :: (term() -> any())
        ) ::
          {:ok, any}
          | {:error, request_error()}
  def run_request(make_request, format_response) do
    request = make_request.()

    with {:ok, %Finch.Response{} = response} <-
           Finch.request(request, Apicult.Finch) |> make_non_200_error(request),
         {:ok, attrs} <- Jason.decode(response.body) do
      {:ok, format_response.(attrs)}
    end
  end

  defp make_non_200_error({:ok, %Finch.Response{status: status} = response}, request)
       when status >= 300,
       do: {:error, %NonSuccessResponseError{request: request, response: response}}

  defp make_non_200_error(other, _), do: other
end
