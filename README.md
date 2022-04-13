# Apicult

A nice api description language that allow for quick api integration.

```
# Get the thing
> http https://example.com/api/the-thing/$thing_id
{
    "title": "The title of the thing",
    ...
}
```

The goal of Apicult is to allow you to write api descriptions by simply copy-and-pasting your shell explorations. That way, you can go straight from trying out an API to building stuff on top, without having to write an API wrapper.



## Binding generators

Once you have an apicult description, you immediatly get an API wrapper in the following languages:
- [Elixir](elixir), using macros to avoid generated files.

## Apicult language overview

An api description file contains a list of endpoints of the form
```
# [[Endpoint name]]
[[Optional default values for variables]]
> [[Http call]]
[[Optionally, result]]
```

The name is a description of the endpoint, and is used to name the generated function.

### Http call

The syntax for http calls replicates [HTTPie](https://httpie.io/cli)'s syntax, so that a working httpie call results in a working call from the generated API wrapper.

```
> http -f POST https://example.com/api/some_endpoint querystring_param==value body_param=value Authorization:"Bearer $access_token"
```
This is an example of every feature, we have:
- `http` is ignore (it's the executable name for httpie).
- `-f` marks that the body will be form-encoded (otherwise, json-encoded is the default).
- `POST` indicates the method to use.
- The url of the call. 
- The querystring parameters, of the form `key==value`.
- The parameters passed in the body, of the form `key=value`.
- The http headers, of the form `key:value`.
- All of these can use `"`, in case you have spaces.
- All of these can contain variables of the form `$variable_name`. They will be translated to function arguments.

### Default values for variables

If the endpoint has variables, it's possible to provide a default value for them, using the following syntax:
```
# An endpoint with pagination
page=1
> http https://example.com/api/get_things page==$page
```

### Result

Optionally, after http call, you can have example result.

```
{
    "title": "The title of the thing",
    ...
}
```

The result is used by the API wrapper to define the result type of the API call.

The format is JSON, with the addition of `...` which can be added to an object to signify that there are more attributes that are less interesting. The API wrapper will keep these attributes around, possibly in an untyped way.


### General configuration

Global variables can be defined before the first endpoint, of the form `variable_name` or `variable_name=default_value`.

These are used by the API wrapper to provide a nice (language-specific) way to define these variables just once. They're great for API credentials.

A default value can be provided in the api definition, which can be used to make things more configurable (to change the host, for example), or simply to hardcode an api token while you're toying with the API.

### Comments

Any line starting with `##` is treated as a comment and ignored.