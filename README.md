My solutions to the exercises from [haskellings](https://github.com/MondayMorningHaskell/haskellings). 

- The tests have been separated from the main code.
- There no need to use the custom `haskellings` executable (see "Running tests" below).

[![](https://github.com/asarkar/haskellings/workflows/CI/badge.svg)](https://github.com/asarkar/haskellings/actions)

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -m <some_word>
```

To run exactly matching tests:
```
./.github/run.sh -m "/<some_word>/"
```

To run a _specific test_:
```
./.github/run.sh -m "/Ch11/evaluates expression/eval/"
```

To run a file containing a `main` method:
```
stack runhaskell <path/to/file> <arg1> <arg2>
```

To run an executable listed in `package.yaml`:
```
stack build
stack exec <name>
```

## License

Released under [Apache License v2.0](LICENSE).