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


{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
import Control.Applicative
import Control.Monad (join, when, unless, liftM)
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ((.=))
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Database.HsPOS.Internal.Http
import Database.HsPOS.Internal.Sqlite (tryCreateTables)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Options.Applicative as O
import Web.Scotty ( middleware, scotty, ScottyM )

data Args = Args
  { optDb     :: String
  , optPort   :: Int
  , optQuiet  :: Bool
  }

-- | This defines a parser for command line arguments.
args :: Parser Args
args = Args <$>
  strOption
  (  long "db"
  <> short 'd'
  <> help "Path to the database store"
  <> showDefault
  <> value "store.db"
  <> metavar "<path to file>"
  ) <*>
  option auto
  (  help "The port number to serve on"
  <> long "port"
  <> metavar "<1-65535>"
  <> short 'p'
  <> showDefault
  <> value 3000
  ) <*>
  switch
  (  long "quiet"
  <> short 'q'
  <> help "Don't print debug info to stdout"
  )

-- | The main entry point.
-- | This function is called by the GHC runtime when the program is run.
main :: IO ()
main = do
  -- Shadow opts with a parsed version of it.
  opts <- execParser opts
  -- Try and create all the tables we want.
  tryCreateTables $ optDb opts
  -- These are lexically bound to the context of the following do block.
  let port = optPort opts
      dbStr = optDb opts
      in
        {- Run the webserver on the given port. The body of this block
           sets policies/options, and then handles setting routes from
           the handlers. -}
        scotty port $ do
          -- Add policies to prevent directory traversal attacks.
          middleware $ staticPolicy (noDots >-> addBase "public")
          -- unless we are using the option -q/--quiet, log to stdout.
          unless (optQuiet opts) $ middleware logStdoutDev

          {- Try these handlers when we recieve a connection. The >>,
             or "then" operator is a way of sequencing multiple
             handlers. This effectively means: try static, then
             allUsersHandler, etc. Each handler handles a route and
             what should be done when a client accesses them.  -}
          
          static >> userHandler     dbStr
                 >> prodHandler     dbStr
                 >> searchHandler   dbStr
                 >> saleHandle      dbStr
                 -- >> prodNameHandler dbStr
  where
    -- Provide the parser to main.
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Run a server for an EPOS system, with a REST API and Database."
      <> O.header "hs-pos -- A Haskell EPOS backend"
      )
