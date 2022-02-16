defmodule Apicult.Generator do
  alias Apicult.Parser

  @doc """
  Generates the code corresponding to an api definition.

  This includes one method per api endpoint, with the variables as arguments.

  If there are configuration variables in the definition, then:
  - A `Client` struct holding them is created ;
  - A `client` function to create the struct is created. It takes keywords corresponding to the variable names, and uses the elixir config for the module as default ;
  - The `Client` struct is added as a first argument for each other methods (defaults to calling `client()`).
  """
  @spec generate_api_bindings(Parser.api()) :: Macro.t()
  def generate_api_bindings({:api, headers, endpoints}) do
    client_keys =
      headers
      |> Enum.map(fn {:config, key, _type, _value} -> key end)

    [
      quote do
        unquote(generate_client(headers))
      end
    ] ++
      (endpoints
       |> Enum.map(&generate_endpoint(&1, client_keys)))
  end

  defp generate_client([]) do
    quote do
    end
  end

  defp generate_client(headers) do
    struct_fields =
      headers
      |> Enum.map(fn {:config, key, _type, value} ->
        if is_nil(value) do
          key
        else
          {key, value}
        end
      end)

    mandatory_keys =
      headers
      |> Enum.flat_map(fn {:config, key, _type, value} ->
        if value == nil do
          [key]
        else
          []
        end
      end)

    quote do
      defmodule Client do
        unquote(
          unless Enum.empty?(mandatory_keys) do
            quote do
              @enforce_keys unquote(mandatory_keys)
            end
          end
        )

        defstruct unquote(struct_fields)
      end

      def client(opts \\ []) do
        config = Application.get_all_env(__MODULE__)
        struct!(Client, Keyword.merge(config, opts))
      end
    end
  end

  @spec generate_endpoint(Apicult.Parser.endpoint(), [atom()]) :: Macro.t()
  defp generate_endpoint(
         {:endpoint, name,
          {:url, _method, interpolated_url, interpolated_querystrings, interpolated_headers,
           interpolated_body} = url, result},
         client_keys
       ) do
    function_name =
      name
      |> String.replace(" ", "_")
      |> String.downcase()
      |> String.to_atom()

    used_variable_names =
      (extract_variables(interpolated_url) ++
         extract_variables(interpolated_querystrings) ++
         extract_variables(interpolated_headers) ++ extract_variables(interpolated_body))
      |> Enum.dedup()

    endpoint_variables =
      used_variable_names
      |> Enum.reject(&Enum.member?(client_keys, &1))
      |> Enum.map(&Macro.var(&1, nil))

    used_client_keys =
      client_keys
      |> Enum.filter(&Enum.member?(used_variable_names, &1))

    client_pattern_match =
      used_client_keys
      |> Enum.map(&{&1, Macro.var(&1, nil)})

    client_arg =
      if Enum.empty?(client_pattern_match) do
        []
      else
        [
          quote do
            %Client{unquote_splicing(client_pattern_match)} \\ __MODULE__.client()
          end
        ]
      end

    response_struct =
      if result != nil do
        {:__aliases__, [alias: false],
         [String.to_atom(Macro.camelize(to_string(function_name)) <> "Response")]}
      else
        nil
      end

    struct_definition =
      if response_struct do
        Apicult.Result.generate_struct(response_struct, result)
      end

    # Spec

    client_spec =
      if Enum.empty?(used_client_keys) do
        []
      else
        quote do
          [
            %Client{
              unquote_splicing(
                Enum.map(used_client_keys, fn key ->
                  quote do
                    {unquote(key), any()}
                  end
                end)
              )
            }
          ]
        end
      end

    arg_specs =
      Enum.map(endpoint_variables, fn _ ->
        quote do
          any
        end
      end)

    result_spec =
      if response_struct do
        response_struct
      else
        quote do
          any()
        end
      end

    spec =
      quote do
        unquote(function_name)(unquote_splicing(client_spec), unquote_splicing(arg_specs)) ::
          unquote(result_spec)
      end

    quote do
      unquote(struct_definition)

      @spec unquote(spec)
      def unquote(function_name)(
            unquote_splicing(client_arg),
            unquote_splicing(endpoint_variables)
          ) do
        # TODO Allow documentation
        make_request = fn ->
          unquote(generate_request(url))
        end

        format_response = fn attrs ->
          unquote(
            if response_struct do
              quote do
                unquote(response_struct).from_map(attrs)
              end
            else
              quote do
                attrs
              end
            end
          )
        end

        case Apicult.Request.run_request(make_request, format_response) do
          {:ok, result} -> result
          {:error, error} -> raise error
        end
      end
    end
  end

  defp generate_request({:url, method, url, querystrings, headers, body}) do
    {body_headers, body_quote} =
      case body do
        nil ->
          {[],
           quote do
             nil
           end}

        {:json, body} ->
          {[{"Content-Type", "application/json"}],
           quote do
             Jason.encode!(unquote(render_interpolated(body)))
           end}

        {:form, body} ->
          {[{"Content-Type", "application/x-www-form-urlencoded"}],
           quote do
             URI.encode_query(unquote(render_interpolated(body)))
           end}
      end

    all_headers = render_interpolated(headers) ++ body_headers

    quote do
      Finch.build(
        unquote(method),
        unquote(render_interpolated_string(url)) <>
          "?" <> URI.encode_query(unquote(render_interpolated(querystrings))),
        unquote(all_headers),
        unquote(body_quote)
      )
    end
  end

  defp extract_variables(l) when is_list(l) do
    Enum.flat_map(l, &extract_variables/1)
  end

  defp extract_variables({:var, v}) do
    [v]
  end

  defp extract_variables({_, v}) do
    extract_variables(v)
  end

  defp extract_variables(_), do: []

  defp render_interpolated(l) when is_list(l) do
    Enum.map(l, &render_interpolated/1)
  end

  defp render_interpolated({key, value}) do
    {key, render_interpolated_string(value)}
  end

  defp render_interpolated_string(interpolated) do
    interpolated
    |> Enum.map(fn
      {:string, s} ->
        s

      {:var, v} ->
        quote do
          "#{unquote(Macro.var(v, nil))}"
        end
    end)
    |> Enum.map(fn
      {:<<>>, [], frags} -> frags
      string when is_bitstring(string) -> [string]
    end)
    |> Enum.concat()
    |> then(fn frags -> {:<<>>, [], frags} end)
  end
end
