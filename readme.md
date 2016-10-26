# TurnStat V3.0.1 Client

Is a command line utility that  enables testing the basic functionality of TurnStat software.


Depedencies: 
====

It uses wreq for the web client part, optparse-applicate for parsing command line options/flags.

Use
====

Compile executable
```shell
stack build
```

To use in conjunction with stack use a double hiphen to pass arguments to the executable.
```shell
stack exec turnstatClient -- --help
```


**TODO:**

- Reuse API key until expires.
- Implement more querying functions.
- Implement tests, to see if a TurnStat installation is working properly.
- Factor into modules (one for querying functions)
- Choose a library to do logging
