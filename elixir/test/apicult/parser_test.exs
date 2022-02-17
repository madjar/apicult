defmodule Apicult.ParserTest do
  use ExUnit.Case
  doctest Apicult.Parser

  test "parses the headers" do
    assert Apicult.Parser.parse(["api_key=2"]) ==
             {:ok, {:api, [{:config, :api_key, :string, "2"}], []}}
  end

  test "parses header without default value" do
    assert Apicult.Parser.parse(["api_key"]) ==
             {:ok, {:api, [{:config, :api_key, :string, nil}], []}}
  end

  test "parses two endpoints" do
    result =
      Apicult.Parser.parse([
        "# Get game data",
        "> http url1",
        "# Get game",
        "> http url2"
      ])

    assert {:ok,
            {:api, [],
             [
               {
                 :endpoint,
                 "Get game data",
                 _,
                 nil,
                 nil
               },
               {
                 :endpoint,
                 "Get game",
                 _,
                 nil,
                 nil
               }
             ]}} = result
  end

  test "treats ## as comments" do
    assert Apicult.Parser.parse(["## something not very valid"]) ==
             {:ok, {:api, [], []}}
  end

  describe "parse_url" do
    test "parses a url with a variable" do
      assert Apicult.Parser.parse_url("http https://$creator.itch.io/$game/data.json") ==
               {:url, :get,
                [
                  {:string, "https://"},
                  {:var, :creator},
                  {:string, ".itch.io/"},
                  {:var, :game},
                  {:string, "/data.json"}
                ], [], [], nil}
    end

    test "parses a url with a querystring" do
      assert Apicult.Parser.parse_url("http https://api.itch.io/games api_key==$api_key") ==
               {:url, :get, [string: "https://api.itch.io/games"],
                [
                  {"api_key", [{:var, :api_key}]}
                ], [], nil}
    end

    test "allows quoting string that contain spaces" do
      assert Apicult.Parser.parse_url("http https://example.com api_key==\"Some thing\"") ==
               {:url, :get, [string: "https://example.com"],
                [
                  {"api_key", [{:string, "Some thing"}]}
                ], [], nil}
    end

    test "parses headers" do
      assert Apicult.Parser.parse_url("http https://example.com Authorization:pouet") ==
               {:url, :get, [string: "https://example.com"], [],
                [{"Authorization", [{:string, "pouet"}]}], nil}
    end

    test "parses body, and assumes POST when body is present" do
      assert Apicult.Parser.parse_url("http https://example.com thing=asd") ==
               {:url, :post, [string: "https://example.com"], [], [],
                {:json, [{"thing", [{:string, "asd"}]}]}}
    end

    test "parses -f for a form body" do
      assert Apicult.Parser.parse_url("http -f https://example.com thing=asd") ==
               {:url, :post, [string: "https://example.com"], [], [],
                {:form, [{"thing", [{:string, "asd"}]}]}}
    end

    test "parses the method" do
      assert Apicult.Parser.parse_url("http POST https://example.com") ==
               {:url, :post, [string: "https://example.com"], [], [], nil}
    end
  end
end
