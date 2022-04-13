defmodule Apicult.Parser do
  @moduledoc """
  Apicult definition parsing
  """

  @typedoc """
  The type of a variable. For now everything is a `:string`
  """
  @type type :: :string

  @typedoc """
  A full parsed apicult definition
  """
  @type api :: {:api, prelude :: [config], endpoints :: [endpoint]}

  @typedoc """
  A config in the prelude, of the form `api_key` or `api_key=something`
  """
  @type config :: {:config, name :: atom(), type :: type, default :: String.t()}

  @typedoc """
  An endpoint
  """
  @type endpoint ::
          {:endpoint, name :: String.t(), url :: url, result :: Apicult.Result.result() | nil,
           expectation :: expectation | nil}

  @typedoc """
  An expectation is a list on line that needs to appear in the resulting request. Used for testing an implementation of Apicult.
  """
  @type expectation :: [String.t()]

  @typedoc """
  An url definition, including method, querystring, headers and body
  """
  @type url ::
          {:url, method :: method, url :: interpolated, querystring :: [querystring],
           headers :: [http_header], body :: body}

  @typedoc """
  An http method
  """
  @type method :: :get | :post

  @typedoc """
  A string containing interpolated variable (eg `https://example.com/$api_key/something`)
  """
  @type interpolated ::
          [{:string, String.t()} | {:var, atom()}]

  @typedoc """
  A key-value pair in a querystring
  """
  @type querystring :: {String.t(), interpolated}

  @typedoc """
  A key-value pair in a http header
  """
  @type http_header :: {String.t(), interpolated}

  @typedoc """
  A http request body
  """
  @type body :: {type :: bodytype, content :: [bodycontent]} | nil

  @typedoc """
  A http body type
  """
  @type bodytype :: :form | :json

  @typedoc """
  A key-value pair in a http body
  """
  @type bodycontent :: {String.t(), interpolated}

  @spec parse_file(binary) :: {:ok, api()}
  @doc """
  Parse a file as an api definition
  """
  def parse_file(filename) do
    File.stream!(filename)
    |> parse()
  end

  @doc """
  Parse the input (an Enum of strings) as an api definition
  """
  @spec parse(Enum.t()) :: {:ok, api()}
  def parse(enum) do
    {raw_prelude, rest} =
      enum
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&String.starts_with?(&1, "##"))
      |> Enum.split_while(&(!new_endpoint(&1)))

    with {:ok, prelude} <- parse_prelude(raw_prelude),
         {:ok, endpoints} <- parse_endpoints(rest),
         do: {:ok, {:api, prelude, endpoints}}
  end

  @spec parse_prelude(Enumerable.t()) :: {:ok, [config]}
  defp parse_prelude(enum) do
    {:ok,
     enum
     |> Enum.flat_map(fn
       "" ->
         []

       config_definition ->
         case String.split(config_definition, "=", parts: 2) do
           [name, value] ->
             [{:config, String.to_atom(name), :string, value}]

           [name] ->
             [{:config, String.to_atom(name), :string, nil}]
         end
     end)}
  end

  @spec parse_endpoints(Enumerable.t()) :: {:ok, [endpoint]}
  defp parse_endpoints(enum) do
    {:ok,
     enum
     |> chunk_on(&new_endpoint/1)
     |> Enum.map(fn ["# " <> name, "> " <> url | rest] ->
       {result, expectation} =
         case rest do
           ["expect" | expectations] -> {nil, expectations |> List.delete("")}
           _ -> {rest |> Enum.join("\n") |> Apicult.Result.parse(), nil}
         end

       {
         :endpoint,
         name,
         parse_url(url),
         result,
         expectation
       }
     end)}
  end

  @spec parse_url(String.t()) :: url
  def parse_url(str) do
    groups =
      str
      |> OptionParser.split()
      |> Stream.map(&parse_url_fragment/1)
      |> Enum.group_by(
        &elem(&1, 0),
        &elem(&1, 1)
      )

    [url] = groups.url
    querystrings = Map.get(groups, :querystring, [])
    headers = Map.get(groups, :header, [])
    body_parts = Map.get(groups, :body, [])
    [body_type] = Map.get(groups, :body_type, [:json])

    body =
      if !Enum.empty?(body_parts) do
        {body_type, body_parts}
      else
        nil
      end

    [method] =
      Map.get(groups, :method, [
        if body do
          :post
        else
          :get
        end
      ])

    {:url, method, url, querystrings, headers, body}
  end

  defp parse_url_fragment(fragment) do
    case fragment do
      "POST" ->
        {:method, :post}

      "-f" ->
        {:body_type, :form}

      "http" ->
        {:ignore, nil}

      "https://" <> _ ->
        {:url, parse_interpolated(fragment)}

      "http://" <> _ ->
        {:url, parse_interpolated(fragment)}

      _ ->
        case String.split(fragment, "==") do
          [key, value] ->
            {:querystring, {key, parse_interpolated(value)}}

          _ ->
            case String.split(fragment, ":") do
              [key, value] ->
                {:header, {key, parse_interpolated(value)}}

              _ ->
                case String.split(fragment, "=") do
                  [key, value] ->
                    {:body, {key, parse_interpolated(value)}}

                  _ ->
                    {:url, parse_interpolated(fragment)}
                end
            end
        end
    end
  end

  @spec parse_interpolated(String.t()) :: interpolated
  defp parse_interpolated(str) do
    Regex.split(~r/\$[[:alpha:]_]+/, str, trim: true, include_captures: true)
    |> Enum.map(fn fragment ->
      if String.starts_with?(fragment, "$") do
        {:var, fragment |> String.trim("$") |> String.to_atom()}
      else
        {:string, fragment}
      end
    end)
  end

  defp chunk_on([], _), do: []

  defp chunk_on([head | tail], f) do
    {this_chunk, rest} = Enum.split_while(tail, &(!f.(&1)))
    [[head | this_chunk] | chunk_on(rest, f)]
  end

  defp new_endpoint(line), do: String.starts_with?(line, "#")
end
