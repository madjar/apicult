defmodule Apicult.Result do
  @type result :: {:result, [field], has_rest?}
  @type field :: {:field, atom(), any()}
  @type has_rest? :: boolean()

  @spec parse(String.t()) :: result

  def parse(""), do: nil

  def parse(str) do
    str
    # Replace '...' with something the jason parser accepts
    |> String.replace("...", ~s'"...": "..."')
    |> Jason.decode!(objects: :ordered_objects)
    |> to_result()
  end

  defp to_result(obj) do
    fields =
      obj.values
      |> Enum.map(fn
        {"...", "..."} -> :rest
        {key, value} -> {:field, String.to_atom(key), process_field(value)}
      end)

    has_rest? = Enum.member?(fields, :rest)

    {:result, List.delete(fields, :rest), has_rest?}
  end

  defp process_field(%Jason.OrderedObject{} = value), do: to_result(value)
  defp process_field(l) when is_list(l), do: {:list, process_field(List.first(l))}
  defp process_field(value), do: value

  def generate_struct(name, {:result, fields, has_rest?} = definition) do
    defined_fields =
      fields
      |> Enum.map(fn {:field, name, _value} -> name end)

    all_fields =
      defined_fields ++
        if has_rest? do
          [:rest]
        else
          []
        end

    nested_results =
      fields
      |> Enum.flat_map(fn
        {:field, name, {:result, _, _} = nested_definition} ->
          [
            generate_struct(
              {:__aliases__, [alias: false], [camelize_atom(name)]},
              nested_definition
            )
          ]

        {:field, name, {:list, {:result, _, _} = nested_definition}} ->
          [
            generate_struct(
              {:__aliases__, [alias: false], [camelize_atom(name)]},
              nested_definition
            )
          ]

        _ ->
          []
      end)

    quote do
      defmodule unquote(name) do
        @definition unquote(Macro.escape(definition))

        @derive {Inspect, only: unquote(defined_fields)}
        defstruct unquote(all_fields)

        unquote_splicing(nested_results)

        def from_map(map),
          do: Apicult.Result.result_to_map(__MODULE__, @definition, map)
      end
    end
  end

  defp camelize_atom(atom) do
    atom
    |> to_string()
    |> Macro.camelize()
    |> String.to_atom()
  end

  def result_to_map(result_struct, {:result, fields, has_rest?}, attrs) do
    struct = struct(result_struct)

    struct_without_rest =
      Enum.reduce(fields, struct, fn {:field, name, value_definition}, acc ->
        case Map.fetch(attrs, Atom.to_string(name)) do
          {:ok, v} ->
            value =
              case value_definition do
                {:result, _, _} = nested_def ->
                  result_to_map(Module.concat(result_struct, camelize_atom(name)), nested_def, v)

                {:list, {:result, _, _} = nested_def} ->
                  Enum.map(
                    v,
                    &result_to_map(
                      Module.concat(result_struct, camelize_atom(name)),
                      nested_def,
                      &1
                    )
                  )

                _ ->
                  v
              end

            %{acc | name => value}

          :error ->
            acc
        end
      end)

    if has_rest? do
      %{struct_without_rest | :rest => attrs}
    else
      struct_without_rest
    end
  end
end
