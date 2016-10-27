# TurnStat V3.0.1 Client

Is a command line utility that  enables testing the basic functionality of TurnStat software.

The current version allows: 

- Create turns
- Call and Finish a Turn
- Get a list a services
- Count waiting tickets
- Set a slot and role for the logged user


Depedencies: 
====

It uses wreq for the web client part, optparse-applicate for parsing command line options/flags.

Use
====

From Inside this project: 

Compile executable
```shell
stack build
```

Test (No tests implemented yet)
```shell
stack test
```

To use in conjunction with stack use a double hiphen to pass arguments to the executable.
```shell
stack exec turnstatClient -- --help
```


**TODO:**

- Reuse API key until expires.
- Implement more querying functions.
- Implement tests, to see if a TurnStat installation is working properly.
- Refactor imports and language extensions with stack
- Choose a library to do logging
