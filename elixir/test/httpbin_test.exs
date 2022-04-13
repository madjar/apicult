defmodule HttpbinTest do
  use ExUnit.Case

  test "A get works" do
    assert Httpbin.example_get("test_value").args.qs == "test_value"
  end

  test "A post with json works" do
    assert Httpbin.post_json("test_value").json.key == "test_value"
  end

  test "A post with form works" do
    assert Httpbin.post_form("test_value").form.key == "test_value"
  end

  test "A redirect is an exception" do
    assert_raise Apicult.Request.NonSuccessResponseError, &Httpbin.redirect/0
  end
end
