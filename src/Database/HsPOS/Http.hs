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
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- HLINT ignore "Use camelCase" -}

-- | This module contains the HTTP handlers for the hs-pos API. These are
--   sequenced together in the Main.hs file, and then run by the Warp server in
--   response to HTTP requests.
module Database.HsPOS.Http where

import Control.Applicative ()
import Control.Exception (try)
import Control.Exception.Base (SomeException)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.ByteString.Char8 (pack)
import Data.Text.Lazy qualified as T
import Data.Time (showGregorian)
import Database.HsPOS.Auth (validateCredentials)
import Database.HsPOS.Session (Session (sessionUUID), randomSession)
import Database.HsPOS.Sqlite
  ( addProd,
    addSession,
    addUser,
    allCUsers,
    allProds,
    bottomProd,
    getCUser,
    getInStock,
    getProd,
    getSession,
    getUser,
    isUsersEmpty,
    makeSale,
    monthlySales,
    monthlyTotalSales,
    percentSalesDiff,
    purgeDb,
    rangeSales,
    removeProd,
    removeUser,
    searchProds,
    searchUsers,
    setStock,
    topProd,
    yearlySales,
  )
import Database.HsPOS.Types (IsOk (IsOk, ok), LoginRequest (requestName), Product (productId, productName, productPrice), ProductSale, User (userName, userPassword), censorUser, saleDate, saleProduct, saleQuantity)
import Network.HTTP.Types (status400, status404)
import Web.Scotty
  ( ScottyM,
    delete,
    file,
    finish,
    get,
    json,
    jsonData,
    liftAndCatchIO,
    param,
    post,
    put,
    status,
    text,
  )

-- Connection handlers --
{- Possibly of note is the pervasive use of the `liftIO` function which may seem
   like an awful lot of boilerplate, but it's necessary to move IO actions into
   the ScottyM monad (which is an IO monad, but not *THE* IO monad)

   If you'd like to know more about monads, I'd recommend checking out
   the [https://wiki.haskell.org/Typeclassopedia](Typeclassopedia) :)
-}

-- | Serve the index page (ie. the main page of the app)
-- | Files are handled by scotty automatically, this is just for explicitness.
static :: ScottyM ()
static = get "/" $ file "./public/index.html"

-- | Handle listing, searching and adding users
userHandler :: FilePath -> ScottyM ()
userHandler dbStr = do
  get "/users/all" $ do
    users <- liftAndCatchIO $ allCUsers dbStr
    json $ map A.toJSON users

  get "/users/:id" $ do
    uid <- param "id"
    user <- liftIO $ try $ getCUser dbStr uid
    case user of
      -- No data, db error or user not found
      -- return code 400: probably a bad request
      Left (_ :: SomeException) -> do
        status status400 >> finish
      -- data is good, send to client
      Right u -> json $ A.toJSON u

  post "/users/" $ do
    user :: User <- jsonData
    -- didn't send us a name so we can't add them
    -- return code 400: probably a bad request.
    when (user.userName == "") (status status400 >> finish)
    result <- liftIO $ addUser dbStr user
    json result

  delete "/users/:id" $ do
    pid <- param "id"
    res <- liftIO $ removeUser dbStr pid
    -- send the result to the client.
    json res

-- | Handle listing, adding and removing products
prodHandler :: FilePath -> ScottyM ()
prodHandler dbStr = do
  get "/prods/:id/" $ do
    pid <- param "id"
    prod <- liftIO $ getProd dbStr pid
    json $ A.toJSON prod

  get "/prods/all" $ do
    prods <- liftIO (allProds dbStr)
    let prodsJSON = map A.toJSON prods
    json $ A.toJSON prodsJSON

  -- Top product ~ the product with the most sales
  get "/prods/top" $ do
    prod <- liftIO (topProd dbStr)
    json prod

  -- Bottom product ~ the product with the least sales
  get "/prods/bottom" $ do
    prod <- liftIO (bottomProd dbStr)
    json prod

  post "/prods/" $ do
    prod :: Product <- jsonData
    -- didn't send us a name so we can't add them
    -- return code 400: probably a bad request.
    when (prod.productName == "") (status status400 >> finish)
    let name = T.unpack $ prod.productName
    let price = prod.productPrice
    -- add the product to the database
    res <- liftIO $ addProd dbStr name price
    -- send the result to the client (i.e. did it work or nah)
    json res

  delete "/prods/:id" $ do
    pid <- param "id"
    res <- liftIO $ removeProd dbStr pid
    -- send the result to the client.
    json res

-- | Handles requests for stock data
stockHandler :: FilePath -> ScottyM ()
stockHandler dbStr = do
  get "/stock/:id" $ do
    pid <- param "id"
    prod <- liftIO $ getProd dbStr pid
    stockData <- liftIO $ getInStock dbStr prod
    json stockData

  get "/stock/all/" $ do
    prods <- liftIO $ allProds dbStr
    stockData <- liftIO $ mapM (getInStock dbStr) prods
    json stockData

  put "/stock/:pid/:num" $ do
    pid <- param "pid"
    num <- param "num"
    res <- liftIO $ setStock dbStr pid num
    json res

-- | Handle sales data requests, and adding sales into the database.
saleHandler :: String -> ScottyM ()
saleHandler dbStr = do
  get "/sales/:y/:m/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    year <- param "y"
    let date = year <> "-" <> month
    sales <- liftIO $ monthlySales dbStr date pid
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales

  -- This really should be JSON but it's a bit late to change it now...
  -- ...shouldn't all of this be JSON?
  get "/sales/:y/:m/to/:y2/:m2/:id/" $ do
    pid <- param "id"
    month <- param "m"
    month2 <- param "m2"
    year <- param "y"
    year2 <- param "y2"
    -- construct & concat them into a date string
    let date1 = year <> "-" <> month
    let date2 = year2 <> "-" <> month2
    -- get the sales from the db
    sales <- liftIO $ rangeSales dbStr date1 date2 pid
    -- Convert the result to json, then send it over HTTP as a reply.
    json sales

  get "/sales/total/:y/:m/:id/" $ do
    {- Fetch the parameters from the url (ie. :y, :m :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    year <- param "y"
    let date = year <> "-" <> month
    sales <- liftIO $ monthlyTotalSales dbStr date pid
    -- Convert the result to json, then send it over HTTP as a reply.
    json sales

  get "/sales/:y/:id/" $ do
    {- Fetch the parameters from the url (ie. :year, :id in the form
       http://localhost:3000/sales/2022/5) -}
    pid <- param "id"
    year <- param "y"
    sales <- liftIO $ yearlySales dbStr year pid
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales

  post "/sales/" $ do
    sale :: ProductSale <- jsonData
    -- return code 400: probably a bad request.
    let pid = sale.saleProduct.productId
    let date = showGregorian $ sale.saleDate
    let quant = sale.saleQuantity
    when (pid == 0 || date == "" || quant == 0) (status status400 >> finish)
    res <- liftIO $ makeSale dbStr date pid quant
    json res

  get "/sales/percent-diff/" $ do
    sales <- liftIO $ percentSalesDiff dbStr
    json sales

-- post "/sales/:y/:m/:d/" $ do
--   prod :: Product <- jsonData
--   year <- param "y"
--   month <- param "m"
--   day <- param "d"
--   let date :: String = year <> "-" <> month <> "-" <> day
--   res <- liftIO $ makeSale dbStr date (prod.productId) 1
--   json res

-- | Handle login requests, session creation, validation and onboarding.
loginHandler :: FilePath -> ScottyM ()
loginHandler dbStr = do
  -- tawa insa means enter in toki pona :)
  post "/tawa-insa/" $ do
    -- We want the JSON body of the request, which should contain the
    -- username and password.
    req :: LoginRequest <- jsonData
    uid <- liftAndCatchIO $ searchUsers dbStr (T.unpack req.requestName)
    -- Try to find the user in the DB.
    user <- liftAndCatchIO $ getUser dbStr (head uid)
    -- Now that we've gotten the request, let's check if it's correct.
    let res = validateCredentials req ((pack . T.unpack) user.userPassword)
    -- If it isn't valid, let's give up. looks like the user sent bad data.
    -- Not worth trying to salvage, so we'll just return a 400.
    unless res (status status400 >> finish)
    -- Now that we know all is good, let's make a session for the user.
    sesh <- liftIO $ randomSession (censorUser user)
    -- Add the session to the session store, discarding the result.
    -- We don't care if it fails, at worst they'll just have to log in again.
    _ <- liftIO $ addSession dbStr sesh
    -- Let's hand the session to the frontend.
    -- They can retain this for later use.
    json sesh

  post "/session/" $ do
    -- Check a user's session
    sesh :: Session <- jsonData
    let sid = sesh.sessionUUID
    -- get our copy of the session.
    sesh' <- liftIO $ getSession dbStr sid
    let eq = sesh == sesh'
    json (eq, sesh')

  get "/onboard/" $ do
    -- are there any users in the database?
    -- if there are, we don't want to present the user with the onboarding page.
    res <- liftIO $ isUsersEmpty dbStr
    let ok' :: IsOk = IsOk {ok = res}
    json ok'

-- | Handle search requests
-- Not sure if i'll use this, it's a bit more efficient to search on the client
-- using a filter/search function, but it's here if i need it.
-- It does come in handy for debugging.
searchHandler :: FilePath -> ScottyM ()
searchHandler dbStr = do
  get "/search/prods/:query" $ do
    query <- param "query"
    result <- liftIO $ try $ searchProds dbStr query

    -- If it's an error, return a 404. Else, return the matching data.
    case result of
      Left (e :: SomeException) -> do
        liftIO $ print e
        status status404
        finish
      Right res -> json res

-- | Purges the entire database, deleting then recreating it!
-- Be very careful with this.
purgeHandler :: FilePath -> ScottyM ()
purgeHandler dbStr = delete "/UNSAFE-PURGE-ALL-CHECK-FIRST-IM-SERIOUS/" $ liftIO (purgeDb dbStr) >> json True
