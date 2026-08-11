# json-parser

Simple json parser written in haskell with interactive querying repl, using parsec, for learning purposes.

## Building
```console
cabal build
```

## Running
```console
cabal run json-parser -- <file-path>
```
where \<file-path> is the json file you want to parse.

# TODOs
- [x] allow array querying with index notation (`foo[0]`)
- [x] add json encoding for pretty priting
- [ ] add support for commands instead of direct queries (`query <obj>`, `update <obj>=<value>`, etc)
- [ ] change the command system for a robust query dsl (something sql-like)
- [ ] parse escaped strings in json (\n, \t, etc)
