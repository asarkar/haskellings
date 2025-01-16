My solutions to the exercises from [haskellings](https://github.com/MondayMorningHaskell/haskellings). 

- The tests have been separated from the main code.
- There no need to use the custom `haskellings` executable (see "Running tests" below).

[![](https://github.com/asarkar/haskellings/workflows/CI/badge.svg)](https://github.com/asarkar/haskellings/actions)

## Progression

The first step is to inspect the introduction modules under the [Basic](src/Basic) package. They contain examples of data structures and Haskell syntax. They do not contain tests and exist to provide a cursory examination of Haskell syntax.

After this, we recommend the following progression of modules:

* [Data](src/Data)
* [Function](src/Function)
* [Recursion](src/Recursion)
* [List](src/List)

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -p '<some_word>'
```

To run a _specific test_:
```
./.github/run.sh -p '<testGroup>.<testCase>'
```
where `testGroup` and `testCase` are the names of the corresponding Tasty namesakes.

To list all tests, in the format accepted by the `-p` flag:
```
./.github/run.sh -l
```

See more about selection patterns [here](https://github.com/UnkindPartition/tasty?tab=readme-ov-file#patterns).

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