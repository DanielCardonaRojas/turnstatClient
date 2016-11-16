module Main where

import Process

----------------------- QUERY DYD TURNSTAT v.3.1 ------------------ 
{-
A client to query a turnstat  server

a basic usage of querying functions can be made using the with session to handle cookies AUTOMATICALLY.
Functions that need an api_key in the header request can be used like this e.g:

    withSession $ \s -> authenticate s >>= getAllServices s
    or
    withSession $ \s -> authenticate s >>= (\k -> runReaderT (callTickets k) (s, defaultClientConfig))

COMPILATION: 
    Compile with stack: stack ghc -- turnstatClient -o turnstatClient

GENERATE DOCUMENTATION:
    stack haddock

This will leave documentation somewhere arount: 
    .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/doc/html/turnstatClient/index.html
-}

main :: IO ()
main = mainProcessing
