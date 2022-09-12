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


module Database.HsPOS.Internal.Http where
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
import Database.HsPOS.Internal.Sqlite (monthlySales, yearlySales)

-- Connection handlers


home :: ScottyM ()
home = get "/" $ file "./public/index.html"


adm :: ScottyM ()
adm = get "/dashboard" $ file "./public/dash.html"

saleHandle :: ScottyM ()
saleHandle = do
  get "/sales/:mm/:yy/:id/" $ do
    id <- param "id"
    mm :: String <- param "mm"
    yy :: String <- param "yy"
    let mmyy = mm <> "-" <> yy
    let a = Unsafe.unsafePerformIO (monthlySales "store.db" mmyy id)
    text $ T.pack $ show a
  
  get "/sales/:yy/id/" $ do
    yy :: String <- param "yy"
    id <- param "id"
    let a = Unsafe.unsafePerformIO (yearlySales "store.db" yy id)
    text $ T.pack $ show a

