# Changelog

## [Unreleased]

### Added
- It's now possible to specify which app to use for the config, using `use ApiCult, "file.api", config_app: :some_app`.

### Changed
- The `use ApiCult, "file.api"` can now be called as `use ApiCult, file: "file.api"`. This allows for additional arguments.

## [0.2.0] - 2022-04-13

### Added
- Endpoint can now take default values for their variables
- Support the special `expect` syntax for the implementation tests (that's only relevant for apicult implementors :) )
- All apicult language features are now tested using standard implementation tests!

### Fixed
- Fix typespec when there's a Client
- Throw nicer exceptions when things fail during the request

## [0.1.0] - 2022-01-10
Initial release