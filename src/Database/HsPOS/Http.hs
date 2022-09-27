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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- HLINT ignore "Use camelCase" -}

module Database.HsPOS.Http where

import Control.Applicative ()
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as T
import Control.Exception (try, catch)
import Database.HsPOS.Auth (validateCredentials, quickHashPassword)
import Database.HsPOS.Sqlite
  ( addProd,
    addUser,
    allCUsers,
    allProds,
    getCUser,
    getProd,
    getUser,
    makeSale,
    monthlySales,
    monthlyTotalSales,
    purgeDb,
    rangeSales,
    searchProds,
    searchUsers,
    yearlySales,
    getSession,
    addSession, isUsersEmpty
  )
import Database.HsPOS.Types (LoginRequest (requestName, requestPass), Product (productId, productName, productPrice), User (userName, userPassword), censorUser)
import Network.HTTP.Types (status400, status418)
import Web.Scotty
  ( ScottyM,
    delete,
    file,
    finish,
    get,
    json,
    jsonData,
    param,
    post,
    status,
    text,
    liftAndCatchIO
  )
import Database.HsPOS.Session ( randomSession, Session (sessionUUID) )
-- Connection handlers

-- | Serve any static pages
static :: ScottyM ()
static = get "/" $ file "./public/index.html"

-- Admin panel
-- get "/dashboard" $ file "./public/dash.html"

searchHandler :: FilePath -> ScottyM ()
searchHandler dbStr = do
  get "/search/prods/:query" $ do
    query <- param "query"
    result <- liftIO $ searchProds dbStr query
    json result

-- | Handle listingm searching and adding users
userHandler :: FilePath -> ScottyM ()
userHandler dbStr = do
  get "/users/all" $ do
    users <- liftIO (allCUsers dbStr)
    json $ map A.toJSON users

  get "/users/:id" $ do
    uid <- param "id"
    user <- liftIO (getCUser dbStr uid)
    json $ A.toJSON user

  post "/users/" $ do
    user :: User <- jsonData
    when (user.userName == "") (status status400 >> finish)
    result <- liftIO $ addUser dbStr user
    json result

prodHandler :: FilePath -> ScottyM ()
prodHandler dbStr = do
  get "/prods/:id/" $ do
    pid <- param "id"
    prod <- liftIO $ getProd dbStr pid
    json $ A.toJSON prod

  get "/prods/all" $ do
    prods <- liftIO (allProds dbStr)
    let prodsJSON = map A.toJSON prods
    status status418
    -- json $ A.toJSON prodsJSON

  post "/prods/" $ do
    prod :: Product <- jsonData
    when (prod.productName == "") (status status400 >> finish)
    let name = T.unpack $ prod.productName
    let price = prod.productPrice
    x <- liftIO $ addProd dbStr name price
    json x

-- | Handle sales data requests, and making new requests.
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

  get "/sales/:y/:m/to/:y2/:m2/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    month2 <- param "m2"
    year <- param "y"
    year2 <- param "y2"
    let date1 = year <> "-" <> month
    let date2 = year2 <> "-" <> month2
    sales <- liftIO $ rangeSales dbStr date1 date2 pid
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales
    
  get "/sales/total/:y/:m/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    year <- param "y"
    let date = year <> "-" <> month
    sales <- liftIO $ monthlyTotalSales dbStr date pid
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales

  get "/sales/:y/:id/" $ do
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022-07/5) -}
    pid <- param "id"
    year <- param "y"
    sales <- liftIO $ yearlySales dbStr year pid
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales

  post "/sales/:y/:m/:d/" $ do
    prod :: Product <- jsonData
    year <- param "y"
    month <- param "m"
    day <- param "d"
    let date :: String = year <> "-" <> month <> "-" <> day
    ok <- liftIO $ makeSale dbStr date (prod.productId) 1
    json ok

-- | Handle login requests.
loginHandler :: FilePath -> ScottyM ()
loginHandler dbStr = do
  -- tawa insa means enter in toki pona :)
  post "/tawa-insa/" $ do
    -- We want the JSON body of the request, which should contain the
    -- username and password.
    req :: LoginRequest <- jsonData
    uid  <- liftAndCatchIO $ searchUsers dbStr (T.unpack req.requestName)
    -- Try to find the user in the DB.
    user <- liftAndCatchIO $ getUser dbStr (head uid)
    -- Now that we've gotten the request, let's check if it's correct.
    liftIO $ print user.userPassword
    liftIO $ print req.requestPass
    pw <- liftIO $ quickHashPassword (T.unpack req.requestPass)
    liftIO $ print pw
    let ok = validateCredentials req ((pack . T.unpack) user.userPassword)
    -- If it isn't valid, let's give up. Return response code 400: bad request.
    unless ok (status status400 >> finish)
    -- Now, let's make a session!
    sesh <- liftIO $ randomSession (censorUser user)
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
    let ok = sesh == sesh'
    json (ok, sesh')

  get "/onboard/" $ do
    ok <- liftIO $ isUsersEmpty dbStr
    json (not ok)

-- | Purges the entire database, deleting then recreating it!
-- Be very careful with this.
purgeHandler :: FilePath -> ScottyM ()
purgeHandler dbStr = delete "/UNSAFE-PURGE-ALL-CHECK-FIRST-IM-SERIOUS/" $ liftIO (purgeDb dbStr) >> json True
