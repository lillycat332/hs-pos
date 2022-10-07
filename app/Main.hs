{-
  hs-pos
  Main.hs
  Created by Lilly Cham

  Copyright (c) 2022, Lilly Cham

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      * Redistributions in binary form must reproduce the above
        copyright notice, this list of conditions and the following
        disclaimer in the documentation and/or other materials provided
        with the distribution.

      * Neither the name of Lilly Cham nor the names of other
        contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- HLINT ignore "Use camelCase" -}

-- | This module contains the main function for the hs-pos executable.
--   It is responsible for parsing command line arguments and running the
--   server itself.
module Main where

import Control.Applicative ((<**>))
import Control.Monad (unless)
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
import qualified Options.Applicative as O
import Web.Scotty (middleware, scotty)

{- Here are our command line arguments. We use this to pass parameters
   in that affect the operation of the entire program. -}
data Args = Args
  { optDb :: FilePath,
    optPort :: Int,
    optQuiet :: Bool
  }

-- | This creates a parser for our command line arguments.
args :: Parser Args
args =
  Args
    <$> strOption
      ( long "db"
          <> short 'd'
          <> help "Path to the database store"
          <> showDefault
          <> value "store.db"
          <> metavar "<path to file>"
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
  -- Try and create all the tables we want.
  tryCreateTables $ optDb popts
  -- These are lexically bound to the context of the following do block.
  let port = optPort popts
      dbStr = optDb popts
   in {- Run the webserver on the given port. The body of this block
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
          >> userHandler dbStr
          >> prodHandler dbStr
          >> searchHandler dbStr
          >> saleHandler dbStr
          >> loginHandler dbStr
          >> stockHandler dbStr
          >> purgeHandler dbStr -- This is last because it's destructive.
  where
    -- Provide the parser to main.
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Run a server for an EPOS system, with a REST API and Database."
            <> O.header "hs-pos -- A Haskell EPOS backend"
        )
