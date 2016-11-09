{-# LANGUAGE OverloadedStrings #-}
module CmdParser where

import Options.Applicative hiding (header, info)
import qualified Options.Applicative as OP

import Types

--------- COMMAND LINE OPTION PARSER ---------

-- | Top level parser
opts :: ParserInfo ClientOptions
opts = 
    OP.info (helper <*> allCommands)
              ( fullDesc
             <> progDesc "Query DyD TurnStat v3.1.1 API"
             <> OP.header "\n\n************************* TURNSTAT CLIENT v0.0.1 ***********************\n\n" )
     where
         allCommands = 
            ClientOptions 
                <$> clientConfigParser
                <*> commandParser

-- | Parses ClientConfiguration i.e target host and credentials
clientConfigParser :: Parser ClientConfig
clientConfigParser = ClientConfig
    <$> strOption
        (  long "host"
        <> metavar "TARGET"
        <> help "Host running TurnStat")
    <*> credentials
    where 
        credentials = 
            (,) <$> strOption 
                        (long "user" <> short 'u' <> metavar "USERNAME" 
                        <> help "User used to connect to turnstat" 
                        <> showDefault <> value "usuario")
                <*> strOption 
                        (long "pass" <> metavar "PASSWORD" 
                        <> help "Users pass used to connect to turnstat" <> value "12345678")


commandParser :: Parser Command
commandParser = 
    subparser (command "create" $ OP.info (helper <*> createTicketCommand) 
                                          (fullDesc <> progDesc "Request a new ticket"))
    <|> subparser (command "dcreate" $ OP.info (helper <*> createDuplicateCommand) 
                                          (fullDesc <> progDesc "Create two tickets at the same time"))
    <|> subparser (command "rcreate" $ OP.info (helper <*> createRandomCommand) 
                                          (fullDesc <> progDesc "Create one or more random tickets"))
    <|> subparser (command "showinfo" $ OP.info (helper <*> showInfoCommand) 
                                          (fullDesc <> progDesc "Get information about service users or slots, defaults to services"))
    <|> subparser (command "periodic" $ OP.info (helper <*> createPeriodicallyCommand) 
                                          (fullDesc <> progDesc "Create tickets forever not exceeding some limit"))
    <|> subparser (command "call" $ OP.info (helper <*> callArbitraryTicketCommand) 
                                          (fullDesc <> progDesc "Call any ticket by id"))
    <|> subparser (command "print" $ OP.info (helper <*> printTicketCommand) 
                                          (fullDesc <> progDesc "Print a ticket with some printer"))
    <|> subparser (command "createuser" $ OP.info (helper <*> createUserCommand) 
                                          (fullDesc <> progDesc "Create a user with a especific role (USER, AUDIT)"))
    where 
        createTicketCommand = CreateTicket 
            <$> option auto (long "origin" <> metavar "ORIGIN" <> help "Origin for ticket")
            <*> option auto (long "service" <> metavar "SERVICE" <> help "Service ID")

        createDuplicateCommand = CreateDuplicate 
            <$> option auto (long "origin" <> metavar "ORIGIN" <> help "Origin for first ticket")
            <*> option auto (long "origin2" <> metavar "ORIGIN" <> help "Origin for second ticket")
            <*> option auto (long "service" <> metavar "SERVICE" <> help "Serive ID")

        createRandomCommand = CreateRandomTicket
            <$> option auto (long "count" <> metavar "COUNT" <> help "How many" <> showDefault <> value 1)

        --showInfoCommand = pure ShowServicesInfo
        showInfoCommand = ShowInfo <$> 
            (flag ServicesInfo SlotsInfo (long "slots" <> showDefault) 
            <|> flag ServicesInfo UsersInfo (long "users") 
            )

        createPeriodicallyCommand = Periodic
            <$> option auto (long "maxlimit" <> metavar "LIMIT" 
                            <> help "Create tickets periodically not exceeding some count")

        callArbitraryTicketCommand = CallArbitrary
            <$> option auto (long "ticket" <> short 't' <> metavar "TICKET_ID" <> help "Call any waiting ticket")

        printTicketCommand = PrintTicket
            <$> option auto (long "ticket" <> short 't' <> metavar "TICKET_ID" <> help "Ticket Id")
            <*> option auto (long "printer" <> short 'p' <> metavar "PRINTER_ID" <> help "Printer Id")

        createUserCommand = CreateUser
            <$> strOption (long "username" <> metavar "USER_LOGIN" <> help "User name and login name")
            <*> strOption (long "pass" <> metavar "LOGIN_PASSWORD" <> help "Login password")
            <*> strOption (long "role" <> metavar "USER_ROLE" <> help "User role")


