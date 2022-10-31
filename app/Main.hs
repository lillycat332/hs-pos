{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Main
-- License: BSD3
-- Stability: Stable
-- Portability: GHC
-- Description: Main module - entry point for hs-pos
--
-- This module contains the main function for the hs-pos executable.
-- It is responsible for parsing command line arguments and running the
-- server itself.
module Main where

import Control.Applicative ((<**>))
import Control.Monad (unless)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HsPOS.Http
  ( loginHandler,
    prodHandler,
    purgeHandler,
    saleHandler,
    searchHandler,
    static,
    stockHandler,
    userHandler,
  )
import Database.HsPOS.Sqlite (tryCreateTables)
import Network.Wai.Middleware.Gzip
  ( GzipFiles (GzipCompress),
    GzipSettings (gzipFiles),
    def,
    gzip,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
  ( addBase,
    noDots,
    staticPolicy,
    (>->),
  )
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    strOption,
    switch,
    value,
  )
import Options.Applicative qualified as O
import Web.Scotty (middleware, scotty)

{- Here are our command line arguments. We use this to pass parameters
   in that affect the operation of the entire program. -}
data Args = Args
  { optDbName :: String,
    optDbHost :: String,
    optDbUser :: String,
    optPort :: Int,
    optQuiet :: Bool
  }

-- | This creates a parser for our command line arguments, and defines the help text
--   shown when an invalid argument or -h/--help is passed.
args :: Parser Args
args =
  Args
    <$> strOption
      ( long "db"
          <> short 'd'
          <> help "Database name"
          <> showDefault
          <> value "db"
          <> metavar "<postgres db name>"
      )
    <*> strOption
      ( long "host"
          <> short 'o'
          <> help "Database Host"
          <> showDefault
          <> value "localhost"
          <> metavar "<postgres db host>"
      )
    <*> strOption
      ( long "user"
          <> short 'u'
          <> help "Database User"
          <> showDefault
          <> value "postgres"
          <> metavar "<postgres db user>"
      )
    <*> option
      auto
      ( help "The port number to serve on"
          <> long "port"
          <> metavar "<1-65535>"
          <> short 'p'
          <> showDefault
          <> value 3000
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Don't print debug info to stdout"
      )

-- | The main entry point.
-- | This is called by the GHC runtime when the program is run.
main :: IO ()
main = do
  -- Instantiate a parsed version of opts.
  popts <- execParser opts

  conn <- connectPostgreSQL ("host=" <> (optDbHost popts) <> " dbname=" <> (optDbName popts) <> " user=" <> (optDbUser popts))
  -- Try and create all the tables + views we want in the database.
  _ <- tryCreateTables conn
  -- These are lexically bound to the context of the following do block.
  let port = optPort popts
   in -- Open a connection to the database.
      -- We clone it so that we can use it in multiple threads.
      {- Run the webserver on the given port. The body of this block
         sets policies/options, and then handles setting routes from
         the handlers. -}
      scotty port $ do
        -- Add policies to prevent directory traversal attacks.
        middleware $ staticPolicy (noDots >-> addBase "public")
        -- Use Gzip, which can shave seconds off load time.
        middleware $ gzip def {gzipFiles = GzipCompress}
        -- unless we are using the option -q/--quiet, log to stdout.
        unless (optQuiet popts) $ middleware logStdoutDev
        {- Try these handlers when we recieve a connection. The >>,
           or monad "then" operator is a way of sequencing multiple
           handlers. This effectively means: try static, then
           allUsersHandler, etc. Each handler handles a route and
           what should be done when a client accesses them.  -}
        static
          >> userHandler conn
          >> prodHandler conn
          >> searchHandler conn
          >> saleHandler conn
          >> loginHandler conn
          >> stockHandler conn
          >> purgeHandler conn -- This is last because it's destructive.
  where
    -- Provide the parser to main.
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Run a server for an EPOS system, with a REST API and Database."
            <> O.header "hs-pos -- A Haskell EPOS backend"
        )
