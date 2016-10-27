# TurnStat V3.0.1 Client

Is a command line utility that  enables testing the basic functionality of TurnStat software.

The current version allows: 

- Create turns
- Call and Finish a Turn
- Get a list a services
- Count waiting tickets
- Set a slot and role for the logged user
- Request to tickets simultaneusly

Which will be display like this when using the --help option:

```shell
************************* TURNSTAT CLIENT v0.0.1 ***********************

Usage: turnstatClient --host TARGET [-u|--user USERNAME] [--pass PASSWORD]
                      (COMMAND | COMMAND | COMMAND | COMMAND | COMMAND |
                      COMMAND)
  Query DyD TurnStat v3.1.1 API

Available options:
  -h,--help                Show this help text
  --host TARGET            Host running TurnStat
  -u,--user USERNAME       User used to connect to turnstat (default: "usuario")
  --pass PASSWORD          Users pass used to connect to turnstat

Available commands:
  create                   Request a new ticket
  dcreate                  Create two tickets at the same time
  rcreate                  Create one or more random tickets
  services                 Get all available services
  periodic                 Create tickets forever not exceeding some limit
  callticket               Call any ticket by id
```

However each command has its own help so for instance: 

```shell
./turnstat create --help
```

will display help for the `create` command


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
