# TurnStat V3.0.1 Client

Is a command line utility that  enables testing the basic functionality of TurnStat software.

The current version allows: 

- Create turns
- Call and Finish a Turn
- Get a list a services
- Count waiting tickets
- Set a slot and role for the logged user
- Request to tickets simultaneusly


### Depedencies: 

It uses wreq for the web client part, optparse-applicate for parsing command line options/flags and HSpec for unit testing.

### Use

From Inside this project: 

**Compile executable**

```shell
stack build
```

To use in conjunction with stack use a double hiphen to pass arguments to the executable.
```shell
stack exec turnstatClient -- --help
stack exec turnstatClient -- create --help
```

**Test**

Run client tests doing:

```shell
stack test
```

**Generate documentation**

```shell
stack haddock
```

This should generate HTML documentation some where in:

> .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/doc



**TODO:**

- Reuse API key until expires.
- Implement more querying functions.
- Implement more tests, to see if a TurnStat installation is working properly.
- Refactor imports and language extensions with stack
- Choose a library to do logging and use.
- Maybe thread Session, so it doesnt have to be passed around all functions, including it in the Rdr type
