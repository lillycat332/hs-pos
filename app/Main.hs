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


{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
import Control.Applicative
import Control.Monad (join, when, unless)
import Data.Aeson ((.=))
import Data.Int
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import qualified Database.HDBC as H
import qualified Options.Applicative as Opt
import qualified System.IO.Unsafe as Unsafe
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Database.HsPOS.Internal.Sqlite
import Database.HsPOS.Internal.Auth
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem -- for constant-time comparison
import Web.Scotty
    ( delete,
      file,
      get,
      html,
      json,
      middleware,
      param,
      post,
      put,
      scotty,
      text,
      ScottyM )


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
  opts <- execParser opts
  let port = optPort opts

  scotty port $ do
    -- Add policies to prevent directory traversal attacks.
    middleware $ staticPolicy (noDots >-> addBase "public")
    -- unless we are using the option -q/--quiet, log to stdout.
    unless (optQuiet opts) $ middleware logStdoutDev

    -- Try these handlers when we recieve a connection
    static >> restHandle >> saleHandle

  where
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Run a server for an EPOS system, with a REST API and Database."
      <> Opt.header "hs-pos -- A Haskell EPOS backend"
      )

-- Connection handlers

-- serve the main static page
static :: ScottyM ()
static = get "/" $ file "./public/index.html"
  -- get "/dashboard" $ file "./public/dash.html"


restHandle :: ScottyM ()
restHandle = do
  get "/users/:u"    $ json $ A.toJSON exampleUser
  post "/users/:u"   $ html "post"
  delete "/users/:u" $ html "delete"
  put "/users/:u"    $ html "put"

  get "/prods/:u"    $ html "get"
  post "/prods/:u"   $ html "post"
  delete "/prods/:u" $ html "delete"
  put "/prods/:u"    $ html "put"


saleHandle :: ScottyM ()
saleHandle = get "/sales/:date/:id/" $ do
  id <- param "id"
  date :: String <- param "date"
  let a = Unsafe.unsafePerformIO (monthlySales "store.db" date id)
  text $ T.pack $ show a

-- SQL Tables


data Product = Product
  { product_id    :: Int
  , product_name  :: T.Text
  , product_price :: Double
  }

data User = User
  { user_id        :: Int
  , user_name      :: T.Text
  , user_password  :: T.Text
  , user_privilege :: Int
  }


data Stock = Stock
  { stock_id   :: Int
  , in_stock   :: T.Text
  }


data Sale = Sale
  { sales_id    :: Int
  , sales_date  :: String
  , number_sold :: Int
  }


data Products_sales_xref = Products_sales_xref
  { xref_product_id :: Int  -- Foreign key to product_id
  , xref_sales_id   :: Int  -- Foreign key to sales_id
  }


data Till = Till
  { till_id   :: Int
  , till_name :: T.Text
  }


data User_till_xref = User_till_xref
  { xref_user_id   :: Int  -- Foreign key to user_id
  , xref_till_id   :: Int  -- Foreign key to till_id
  }

exampleUser :: User
exampleUser = User 1 "admin" "password" 1

-- Marshalling
-- To JSON


instance A.ToJSON User where
  toJSON (User user_id user_name user_password user_privilege) =
    A.object [ "id" .= user_id
                 , "name" .= user_name
                 , "passwd" .= user_password
                 , "privilege" .= user_privilege
                 ]

instance A.ToJSON Product where
  toJSON (Product product_id product_name product_price) =
    A.object [ "id" .= product_id
             , "name" .= product_name
             , "price" .= product_price
             ]

instance A.ToJSON Stock where
  toJSON (Stock stock_id in_stock) =
    A.object [ "id" .= stock_id
             , "in_stock" .= in_stock
             ]

instance A.ToJSON Sale where
  toJSON (Sale sales_id sales_date number_sold) =
    A.object [ "id" .= sales_id
             , "date" .= sales_date
             , "number_sold" .= number_sold
             ]

instance A.ToJSON Till where
  toJSON (Till till_id till_name) =
    A.object [ "id" .= till_id
             , "name" .= till_name
             ]
