defmodule HttpbinTest do
  use ExUnit.Case

  defmodule Httpbin do
    use Apicult, "test/httpbin.api"
  end

  test "A get works" do
    assert Httpbin.example_get("test_value").args.qs == "test_value"
  end

  test "A redirect is an exception" do
    assert_raise Apicult.Request.NonSuccessResponseError, &Httpbin.redirect/0
  end
end
