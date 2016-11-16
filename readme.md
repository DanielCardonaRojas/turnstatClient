# TurnStat V3.0.1 Client

Is a command line utility that  enables testing the basic functionality of DyD TurnStat software.

The current version allows: 

- Create turns/tickes
- Call and Finish a Turn
- Get a list a services
- Count waiting tickets
- Set a slot and role for the logged user
- Request to tickets simultaneusly

Which will be display like this when using the --help option:

```shell
************************* TURNSTAT CLIENT v0.0.1 ***********************

Usage: turnstatClient HOST [-u|--user USERNAME] [--pass PASSWORD] (COMMAND |
                      COMMAND | COMMAND | COMMAND | COMMAND | COMMAND | COMMAND
                      | COMMAND)
  Query DyD TurnStat v3.1.1 API

Available options:
  -h,--help                Show this help text
  -u,--user USERNAME       User used to connect to turnstat (default: "usuario")
  --pass PASSWORD          Users pass used to connect to turnstat

Available commands:
  create                   Request a new ticket
  dcreate                  Create two tickets at the same time
  rcreate                  Create one or more random tickets
  showinfo                 Get information about service users or slots,
                           defaults to services
  periodic                 Create tickets forever not exceeding some limit
  call                     Call any ticket by id
  print                    Print a ticket with some printer
  createuser               Create a user with a especific role (USER, AUDIT)
```

However each command has its own help so for instance: 

```shell
./turnstat create --help
```

will display help for the `create` command


### Depedencies: 

It uses [wreq](http://www.serpentine.com/wreq/tutorial.html) for the web client part, 
[optparse-applicative](https://github.com/pcapriotti/optparse-applicative) for parsing command line options/flags and [HSpec](http://hspec.github.io/) for unit testing.

This proyect also uses a custom package hosted here in this same Github account, called [GenericUtils](https://github.com/DanielCardonaRojas/GenericUtils). 
This is a just a simple package containing easy generic function definitions.

### Use

From Inside this project: 

**Get a local version of stack** 

```shell
stack setup
```

**Compile executable**


```shell
stack build
```

To use in conjunction with stack use a double hiphen to pass arguments to the executable.
```shell
stack exec turnstatClient -- --help
stack exec turnstatClient -- create --help
```

After this the excutable can be found in .stack-work/dist/<YOUR_OS>/Cabal-x.x.x.x/build/turnstatClient

**Test**

Run client tests doing: 

```shell
stack test
```

or 

```shell
stack test turnstatClient
```

Note these test arent actually testing mucho about this code itself but server response expectations.

**Generate documentation**

```shell
stack haddock
```

This should generate HTML documentation some where in:

> .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/doc


**Install**

To use from outsite of this proyect and develop:

Move to location where binary is put:
For example: `.stack-work/install/x86_64-osx/lts-7.5/8.0.1/bin/turnstatClient`

Create a symlink to location in $PATH e.g /usr/local/bin or /opt/local/bin

```shell
sudo ln -s $PWD/turnstatClient /opt/local/bin/turnstatClient
```

**TODO:**

- Reuse API key until expires.
- Refresh cookie so calling different hosts works fine.
- Implement more querying functions.
- Implement more tests, to see if a TurnStat installation is working properly.
- Refactor imports and language extensions with stack
- Choose a library to do logging and use.
- Maybe thread Session, so it doesnt have to be passed around all functions, including it in the Rdr type
- Separate testing concerns, this library vs response expectations.
- Reduce binary size, test binary distribution. (edit the .cabal ghc flags removing -dynamic and just strip the binary: strip turnstatClient)
