# Guide for the implementor

## Write a parser

First, you need to write a parser for the apicult description language. For now, the best description of the language can be found in the [README](./README.md).

If you want fast feedback, you can parse `implementation_tests.api`, which contains most options of the language.

At this stage, I recommend ignoring the json result when parsing, so you have an MVP earlier.

## Generate requests

Once you have parsed the description of the api, you can generate requests from it. There are two options here:

- Generate requests dynamically at runtime, from an endpoint definition and a map of variables. This is required if you want your program to work with api descriptions that are not known at runtime, or work in a language that doesn't allow you to do fancy things at compile-time. The additional complexity here is that you'll have to handle variables as a map, and their the resolution of string interpolation manually.
- Use macros or code generation to create code that generates the requests. This is required if you want to generate an api wrapper that can be used like it was hand-written (with variables being function parameters, and nice docs).

Once you have implemented one of the two, you can write tests to check that the generated requests match the expectations in `implementation_tests.api`. Since every http library has it's own tiny differences (specific headers, encoding, ordering), it's okay to touch up the expectation before comparing.

Optionally, you can implement the dynamic generation, then the macro in term of the dynamic generation, if you want to work on the logic before diving into macros. You can then refactor things to move as much things as you want to compile time.

## Tie it all together

Write some code generation that parses the api definition, then for each endpoint creates a function that build a request and sends it.

## Nice result types