# Apicult

An API binding generator based on a nice-to-write format

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

## Format description

TODO

## Elixir bindings

Use as follow:
```
defmodule Itch do
  use Apicult, "itch.api"
end
```

This will fill the `Itch` module with the following:
- A function for each endpoint. These functions create a HTTP request based on the endpoint definition, make the call.
- If a result is specified for an endpoint, corresponding structs are created, and will be used to parse the response.
  - If the result definition contains "...", then these struct have a `rest` field, that contains the entire json object, and is hidden from `inspect`.
- If the response is a 3xx, the Location header is returned.
- If there are some global config values, a `Client` struct that holds the config. It is passed as first argument to all functions, though you can skip it if all config fields have default values.
- You can set default values for `Client` in the specification, or in the corresponding elixir config, eg `config Itch, api_key: System.get_env "ITCH_KEY"`