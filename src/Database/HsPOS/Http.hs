{- 
  hs-pos
  Http.hs
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


{-# LANGUAGE Trustworthy, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, OverloadedStrings, OverloadedRecordDot #-}
{- HLINT ignore "Use camelCase" -}

module Database.HsPOS.Http where
import Control.Applicative ()
import Control.Monad (join, when, unless, liftM)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Network.HTTP.Types ( status400 )
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase
                                     , noDots
                                     , staticPolicy
                                     , (>->))
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throw, try)
import Web.Scotty ( delete
                  , file
                  , get
                  , html
                  , json
                  , jsonData
                  , param
                  , post
                  , put
                  , scotty
                  , status
                  , text
                  , next
                  , finish
                  , ScottyM
                  , ActionM )
import Database.HsPOS.Sqlite
    ( makeSale,
      monthlyTotalSales,
      monthlySales,
      rangeSales,
      yearlySales,
      addProd,
      allCUsers,
      getCUser,
      getProd,
      allProds,
      searchProds, addUser, purgeDb )
import Database.HsPOS.Types ( Request, User, Product (product_name, product_price, product_id) )
import Database.HsPOS.Auth ( validateCredentials )

-- Connection handlers

-- | Serve any static pages
static :: ScottyM ()
static = get "/" $ file "./public/index.html"
  -- Admin panel
  -- get "/dashboard" $ file "./public/dash.html"

searchHandler :: FilePath -> ScottyM ()
searchHandler dbStr = do
  get "/search/prods/:query" $ do
    query  <- param "query"
    result <- liftIO $ searchProds dbStr query
    json result  

-- | Handle listingm searching and adding users
userHandler :: FilePath -> ScottyM ()
userHandler dbStr = do
  get "/users/all" $ do
    users <- liftIO (allCUsers dbStr)
    json $ map A.toJSON users

  get "/users/:id" $ do
    id   <- param "id"
    user <- liftIO (getCUser dbStr id)
    json $ A.toJSON user

  post "/users/" $ do
    user ::User <- jsonData
    result <- liftIO $ addUser dbStr user
    json result

prodHandler dbStr = do
  get "/prods/:id/" $ do
    id    <- param "id"
    prod  <- liftIO $ getProd dbStr id
    json $ A.toJSON prod

  get "/prods/all" $ do
    prods <- liftIO (allProds dbStr)
    let prodsJSON = map A.toJSON prods
    json $ A.toJSON prodsJSON

  post "/prods/" $ do
    prod :: Product <- jsonData
    let name  = T.unpack $ prod.product_name
    let price = prod.product_price
    x        <- liftIO $ addProd dbStr name price
    json x

-- | Handle sales data requests, and making new requests.
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
    json sales

  get "/sales/:y/:m/to/:y2/:m2/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    id       <- param "id"
    month    <- param "m"  
    month2   <- param "m2" 
    year     <- param "y"  
    year2    <- param "y2"
    let date1 = year  <> "-" <> month
    let date2 = year2 <> "-" <> month2
    sales    <- liftIO $ rangeSales dbStr date1 date2 id
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales

  get "/sales/total/:y/:m/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    id      <- param "id"
    month   <- param "m"
    year    <- param "y"
    let date = year <> "-" <> month
    sales   <- liftIO $ monthlyTotalSales dbStr date id
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales
    
  get "/sales/:y/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022-07/5) -}
    id    <- param "id"
    year  <- param "y"
    sales <- liftIO $ yearlySales dbStr year id
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales

  post "/sales/:y/:m/:d/" $ do
    prod  :: Product <- jsonData
    year  <- param "y"
    month <- param "m"
    day   <- param "d"
    let date :: String = year <> "-" <> month <> "-" <> day
    ok    <- liftIO $ makeSale dbStr date (prod.product_id) 1
    json ok

-- | Handle login requests.
loginHandle :: FilePath -> ScottyM ()
loginHandle dbStr = do
  post "/tawa-insa/" $ do
    -- We want the JSON body of the request, which should contain the
    -- username and password.
    req :: Request <- jsonData
                       
    -- Now that we've gotten the request, let's make sure it's valid.
    let ok = validateCredentials req

    unless ok (status status400 >> finish)
    text "I'm in."

purgeHandler :: FilePath -> ScottyM ()
purgeHandler dbStr = delete "/UNSAFE-PURGE-ALL-CHECK-FIRST-IM-SERIOUS/" $ liftIO (purgeDb dbStr) >> json True
    
