# Apicult — Elixir

An API binding generator based on a nice-to-write description language

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `apicult` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:apicult, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/apicult>.

<!-- MDOC !-->

## Example

```
access_token

# Example get
> http https://httpbin.org/get qs==$qs Authorization:"Bearer $access_token"
{
    "args": {
        "qs": "something"
    },
    "headers": {
        "Authorization": "Bearer something",
        ...
    },
    ...
}
```

```
defmodule Example do
  use Apicult, "example.api"
end
```

This generates the `client` and `example_get` methods, and the `Client` and `ExampleGetResponse`, which can be used as follows:

```
iex> client = Example.client access_token: "SECRET"
%Example.Client{access_token: "SECRET"}

iex> response = Example.example_get client, "querystring_value"
#Example.ExampleGetResponse<
  args: %Example.ExampleGetResponse.Args{qs: "querystring_value"},
  headers: #Example.ExampleGetResponse.Headers<
    Authorization: "Bearer SECRET",
    ...
  >,
  ...
>

iex> response.headers.rest
%{
  "Authorization" => "Bearer SECRET",
  "Host" => "httpbin.org",
  "User-Agent" => "mint/1.4.0"
}
```

## How to use

Use as follow:
```
defmodule Example do
  use Apicult, "example.api"
end
```

This will fill the `Example` module with functions for all the endpoints. These functions will make the http call and return the result.

In case the http response is a 3xx, the content of the Location header is returned.

### Result

If a result is specified for an endpoint, corresponding structs are created, and will be used to parse the response.

If the result definition contains "...", then these struct have a `rest` field, that contains the entire json object, and is hidden from `inspect`.

### Global configuration

If there are some global config values, a `Client` struct that holds the config. It is passed as first argument to all functions, though you can skip it if all config fields have default values.

You can set default values for `Client` in the specification, or in the corresponding elixir config, eg `config Itch, api_key: System.get_env "ITCH_KEY"`

## Implementation tests

There's a hidden piece of syntax in the apicult format to make the work of implementors easier: the result may instead start with the `expect` keyword to define an expectation. For example:

```
# Basic request
> http http://example.com/get
expect
GET /get HTTP/1.1
Host: example.com
```

The expectations contains lines that are expected to be in the request generated for this endpoint. The [implementation_tests.api](./implementation_tests.api) file contains such expectations for all apicult features. This means that once you have written your parser, and your code that generates http requests (which you very likely need to make http requests), then you get tests for all language features for free!

You might need to write code to convert an http request into its representation, but that should be straightforwared. You can use [the elixir code for these tests](./elixir/test/implementation_test.exs) as an example.
