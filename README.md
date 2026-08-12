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

## DSL
### Commands
- `get <object-path>` - returns the value of the path given. does not change the current position.
- `walk <object-path>` - change the current posision to the given path.
- `show` - show all the content of the current position.
- `set <object-path> <value>` - sets the object at the relative path to the value given.
- `set $<var-name> <value>` - sets a variable to a value. this value can be a path itself.

### Variables
- `$root` - root of the json object
- `$current` - refers to the object at the current position.

### Ideas
- Another functions/commands like `filter` that could operate on the data itself.
- Allow piping of functions with `|`. A function like `filter` could be made specifically to use this way i.e instead of having to pass the object as the argument, you could pipe it after getting from `get`: `get command.flags | filter ($value == true)`

# TODOs
- [x] allow array querying with index notation (`foo[0]`)
- [x] add json encoding for pretty priting
- [x] manage a current position on the json
- [x] add support for commands instead of direct queries
- [x] show collapsed version of json when not calling `show`
- [ ] change the command system for a robust query dsl (something sql-like)
- [ ] parse escaped strings in json (\n, \t, etc)
