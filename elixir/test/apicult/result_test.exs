defmodule Apicult.ResultTest do
  use ExUnit.Case
  doctest Apicult.Result

  describe "parse" do
    test "parses a result without a rest" do
      assert """
             {
               "id": 1170978
             }
             """
             |> Apicult.Result.parse() ==
               {:result, [{:field, :id, 1_170_978}], false}
    end

    test "parses a result with a rest" do
      assert """
             {
               "id": 1170978,
               ...
             }
             """
             |> Apicult.Result.parse() ==
               {:result, [{:field, :id, 1_170_978}], true}
    end

    test "parses a result with an object" do
      assert """
             {
               "result": {
                 "id": 42
               }
             }
             """
             |> Apicult.Result.parse() ==
               {:result,
                [
                  {:field, :result, {:result, [{:field, :id, 42}], false}}
                ], false}
    end

    test "parses a result with a list" do
      assert """
             {
               "result": [42]
             }
             """
             |> Apicult.Result.parse() ==
               {:result, [{:field, :result, {:list, 42}}], false}
    end

    test "parses a result with a list of objects" do
      assert """
             {
               "result": [{
                "id": 42
              }]
             }
             """
             |> Apicult.Result.parse() ==
               {:result, [{:field, :result, {:list, {:result, [{:field, :id, 42}], false}}}],
                false}
    end
  end

  describe "generate_struct" do
    test "generates a struct for result with rest" do
      result =
        """
        {
          "id": 1170978,
          ...
        }
        """
        |> Apicult.Result.parse()

      expected =
        quote context: Apicult.Result do
          defmodule Pouet do
            @definition {:result, [{:field, :id, 1_170_978}], true}
            @derive {Inspect, only: [:id]}
            defstruct [:id, :rest]

            def from_map(map),
              do: Apicult.Result.result_to_map(__MODULE__, @definition, map)
          end
        end
        |> Macro.to_string()

      assert Apicult.Result.generate_struct(Pouet, result) |> Macro.to_string() == expected
    end

    test "generates a nested struct" do
      result =
        """
        {
          "results": [{"id": 42}]
        }
        """
        |> Apicult.Result.parse()

      expected =
        quote context: Apicult.Result do
          defmodule Pouet do
            @definition {:result,
                         [{:field, :results, {:list, {:result, [{:field, :id, 42}], false}}}],
                         false}
            @derive {Inspect, only: [:results]}
            defstruct [:results]

            defmodule Results do
              @definition {:result, [{:field, :id, 42}], false}
              @derive {Inspect, only: [:id]}
              defstruct [:id]

              def from_map(map) do
                Apicult.Result.result_to_map(__MODULE__, @definition, map)
              end
            end

            def from_map(map) do
              Apicult.Result.result_to_map(__MODULE__, @definition, map)
            end
          end
        end
        |> Macro.to_string()

      assert Apicult.Result.generate_struct(Pouet, result) |> Macro.to_string() ==
               expected
    end
  end

  describe "result_to_map" do
    test "converts a nested map to the proper struct" do
      # Copied from the previous test, with the @derive removed
      defmodule Pouet do
        @definition {:result,
                     [{:field, :results, {:list, {:result, [{:field, :id, 42}], false}}}], false}
        defstruct [:results]

        defmodule Results do
          @definition {:result, [{:field, :id, 42}], false}
          defstruct [:id]

          def from_map(map) do
            Apicult.Result.result_to_map(__MODULE__, @definition, map)
          end
        end

        def from_map(map) do
          Apicult.Result.result_to_map(__MODULE__, @definition, map)
        end
      end

      map = %{"results" => [%{"id" => 42}]}

      assert List.first(Pouet.from_map(map).results).id == 42
    end
  end
end
