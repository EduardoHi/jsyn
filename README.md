# jsyn

json + synthesis.

Jsyn is a tool for Programming by Example: given a set of input-output examples,
it tries to create a javascript program that can perform such transformation.
For example:

Given, a single input example pair:

```json
[
  {
    "input": [
      {
        "key": 1,
        "host": "<host1>",
        "name": "<name1>"
      },
      {
        "key": 2,
        "host": "<host2>",
        "name": "<name2>"
      }
    ],
    "output": ["<name1>", "<name2>"]
  }
]
```

it outputs:

```javascript
function program(xs) {
  return xs.map(x => {
    return x.name;
  });
}
```

For a more detailed explanation of what it is and how it works,
read the pdf `hidalgo20-report.pdf`.

to run the program:

```shell
$ cabal run jsyn -- <example file>
```

for example:

```shell
$ cabal run jsyn -- tests/test8.json
```

other commands:

```shell
$ cabal test
```

To run all tests, and:

```shell
$ cabal bench
```

to run microbenchmarks
