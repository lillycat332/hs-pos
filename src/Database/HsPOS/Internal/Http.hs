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
import Control.Monad (join, when, unless, liftM)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
    ( delete,
      file,
      get,
      html,
      json,
      param,
      post,
      put,
      scotty,
      text,
      ScottyM )
import Database.HsPOS.Internal.Sqlite
import Database.HsPOS.Internal.Types

-- Connection handlers

-- | Serve any static pages
static :: ScottyM ()
static = get "/" $ file "./public/index.html"
  -- Admin panel
  -- get "/dashboard" $ file "./public/dash.html"

--searchHandler :: FilePath -> ScottyM ()
searchHandler dbStr = do
  get "/search/prods/:query" $ do
    query  <- param "query"
    result <- liftIO $ searchProds dbStr query
    json $ result  

-- | Returns a list of all users/products over HTTP
userHandler :: FilePath -> ScottyM ()
userHandler dbStr = do
  get "/users/all" $ do
    users <- liftIO (allUsers dbStr)
    let usersJSON = map A.toJSON
          $ map (\(x,y,z) -> CensoredUser x (T.pack y) z) users
    json $ A.toJSON usersJSON

  get "/users/:id" $ do
    id   <- param "id"
    user <- liftIO (getUser dbStr id)
    let userJSON = A.toJSON
          $ (\(x,y,z) -> CensoredUser x (T.pack y) z) user in 
            json $ A.toJSON userJSON

prodHandler dbStr = do
  get "/prods/:id/" $ do
    id <- param "id"
    x  <- liftIO $ getProd dbStr id
    json $ (\(id,name,price) ->
           A.toJSON $ Product id (T.pack name) price) x

  get "/prods/all" $ do
    prods <- liftIO (allProds dbStr)
    let prodsJSON = map A.toJSON
          $ map (\(x,y,z) -> Product x (T.pack y) z) prods
    json $ A.toJSON prodsJSON

  post "/prods/" $ do
    name  <- param "name"
    price <- param "price"
    x     <- liftIO $ addProd dbStr name price
    json x

saleHandle :: String -> ScottyM ()
saleHandle dbStr = do
  get "/sales/:y/:m/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    id      <- param "id"
    month   <- param "m"
    year    <- param "y"
    let date = year <> "-" <> month
    sales   <- liftIO $ monthlySales dbStr date id
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales
    
  get "/sales/:y/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022-07/5) -}
    id    <- param "id"
    year  <- param "y"
    
    sales <- liftIO $ yearlySales dbStr year id
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales
