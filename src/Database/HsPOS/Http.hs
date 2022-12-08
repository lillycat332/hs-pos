{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module: Database.HsPOS.Http
-- License: BSD3
-- Stability: Unstable
-- Portability: GHC
-- Description: HTTP handlers for the hs-pos API.
-- These HTTP handlers are sequenced together in the Main.hs file, and then
-- run by the Warp server in response to HTTP requests.
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
import Database.HDBC (clone)
import Database.HDBC.PostgreSQL (Connection)
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
    leastSqProd,
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
   the [https://wiki.haskell.org/Typeclassopedia](Typeclassopedia) c:
-}

-- | Serve the index page (ie. the main page of the app)
-- | Files are handled by scotty automatically, this is just for explicitness.
static :: ScottyM ()
static = get "/" $ file "./public/index.html"

-- | Handle listing, searching and adding users
userHandler :: Connection -> ScottyM ()
userHandler db = do
  get "/users/all" $ do
    conn <- liftAndCatchIO $ clone db
    users <- liftAndCatchIO $ allCUsers conn
    json $ map A.toJSON users

  get "/users/:id" $ do
    conn <- liftAndCatchIO $ clone db
    uid <- param "id"
    user <- liftIO $ try $ getCUser conn uid
    case user of
      -- No data, db error or user not found
      -- return code 400: probably a bad request
      Left (_ :: SomeException) -> do
        status status400 >> finish
      -- data is good, send to client
      Right u -> json $ A.toJSON u

  post "/users/" $ do
    conn <- liftAndCatchIO $ clone db
    user :: User <- jsonData
    -- didn't send us a name so we can't add them
    -- return code 400: probably a bad request.
    when (user.userName == "") (status status400 >> finish)
    result <- liftIO $ addUser conn user
    json result

  delete "/users/:id" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "id"
    res <- liftIO $ removeUser conn pid
    -- send the result to the client.
    json res

-- | Handle listing, adding and removing products
prodHandler :: Connection -> ScottyM ()
prodHandler db = do
  -- Select a product by ID
  get "/prods/:id/" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "id"
    prod <- liftIO $ getProd conn pid
    json $ A.toJSON prod

  -- All products
  get "/prods/all" $ do
    conn <- liftAndCatchIO $ clone db
    prods <- liftIO (allProds conn)
    let prodsJSON = map A.toJSON prods
    json $ A.toJSON prodsJSON

  -- Top product ~ the product with the most sales
  get "/prods/top" $ do
    conn <- liftAndCatchIO $ clone db
    prod <- liftIO (topProd conn)
    json prod

  -- Bottom product ~ the product with the least sales
  get "/prods/bottom" $ do
    conn <- liftAndCatchIO $ clone db
    prod <- liftIO (bottomProd conn)
    json prod

  -- Add a product
  post "/prods/" $ do
    conn <- liftAndCatchIO $ clone db
    prod :: Product <- jsonData
    -- didn't send us a name so we can't add them
    -- return code 400: probably a bad request.
    when (prod.productName == "") (status status400 >> finish)
    let name = T.unpack $ prod.productName
    let price = prod.productPrice
    -- add the product to the database
    res <- liftIO $ addProd conn name price
    -- send the result to the client (i.e. did it work or nah)
    json res

  delete "/prods/:id" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "id"
    res <- liftIO $ removeProd conn pid
    -- send the result to the client.
    json res

-- | Handles requests for stock data
stockHandler :: Connection -> ScottyM ()
stockHandler db = do
  get "/stock/:id" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "id"
    prod <- liftIO $ getProd conn pid
    stockData <- liftIO $ getInStock conn prod
    json stockData

  get "/stock/all/" $ do
    conn <- liftAndCatchIO $ clone db
    prods <- liftIO $ allProds conn
    stockData <- liftIO $ mapM (getInStock conn) prods
    json stockData

  put "/stock/:pid/:num" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "pid"
    num <- param "num"
    res <- liftIO $ setStock conn pid num
    json res

-- | Handle sales data requests, and adding sales into the database.
saleHandler :: Connection -> ScottyM ()
saleHandler db = do
  -- At the top because of order of execution issues
  -- Fetch predicted sales for a product
  get "/sales/predicted/:id/:num" $ do
    conn <- liftAndCatchIO $ clone db
    pid :: Integer <- param "id"
    num :: Integer <- param "num"
    -- If the number of sales inputted is zero or negative we can't provide
    -- meaningful data. This is not a bad request, but we can't do anything useful.
    -- Send back 0, then bail out.
    when (num <= 0) (text "0" >> finish)
    salesPrev <- liftIO $ leastSqProd conn pid num
    -- Don't send invalid numbers to the client
    when (isInfinite salesPrev || isNaN salesPrev || salesPrev < 0) (text "0" >> finish)
    -- We have to divide by 24 because we're predicting sales per day but leastSqProd
    -- returns sales per hour.
    text . T.pack . show $ salesPrev / 24.0

  -- get sales for a month
  get "/sales/:y/:m/:id/" $ do
    conn <- liftAndCatchIO $ clone db
    {- Fetch the parameters from the url (ie. :date, :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    year <- param "y"
    let date = year <> "-" <> month <> "-01"
    sales <- liftIO $ monthlySales conn date pid
    -- Convert the result to text, then send it over HTTP as a reply.
    json sales

  -- get sales for a year range
  get "/sales/:y/:m/to/:y2/:m2/:id/" $ do
    conn <- liftAndCatchIO $ clone db
    pid <- param "id"
    month <- param "m"
    month2 <- param "m2"
    year <- param "y"
    year2 <- param "y2"
    -- construct & concat them into a date string
    let date1 = year <> "-" <> month
    let date2 = year2 <> "-" <> month2
    -- get the sales from the db
    sales <- liftIO $ rangeSales conn date1 date2 pid
    -- Convert the result to json, then send it over HTTP as a reply.
    json sales

  -- Get the total sales for a year
  get "/sales/total/:y/:m/:id/" $ do
    conn <- liftAndCatchIO $ clone db
    {- Fetch the parameters from the url (ie. :y, :m :id in the form
       http://localhost:3000/sales/2022/07/5) -}
    pid <- param "id"
    month <- param "m"
    year <- param "y"
    let date = year <> "-" <> month
    sales <- liftIO $ monthlyTotalSales conn date pid
    -- Convert the result to json, then send it over HTTP as a reply.
    json sales

  get "/sales/:y/:id/" $ do
    conn <- liftAndCatchIO $ clone db
    {- Fetch the parameters from the url (ie. :year, :id in the form
       http://localhost:3000/sales/2022/5) -}
    pid <- param "id"
    year <- param "y"
    sales <- liftIO $ yearlySales conn year pid
    -- Convert the result to text, then send it over HTTP as a reply.
    text $ T.pack $ show sales

  post "/sales/" $ do
    conn <- liftAndCatchIO $ clone db
    sale :: ProductSale <- jsonData
    let pid = sale.saleProduct.productId
    let date = showGregorian $ sale.saleDate
    let quant = sale.saleQuantity
    when (pid == 0 || date == "") (status status400 >> finish)
    res <- liftIO $ makeSale conn date pid quant
    json res

  get "/sales/percent-diff/" $ do
    conn <- liftAndCatchIO $ clone db
    sales <- liftIO $ percentSalesDiff conn
    json sales

-- post "/sales/:y/:m/:d/" $ do
--   prod :: Product <- jsonData
--   year <- param "y"
--   month <- param "m"
--   day <- param "d"
--   let date :: String = year <> "-" <> month <> "-" <> day
--   res <- liftIO $ makeSale conn date (prod.productId) 1
--   json res

-- | Handle login requests, session creation, validation and onboarding.
loginHandler :: Connection -> ScottyM ()
loginHandler db = do
  -- tawa insa means enter in toki pona :)
  post "/tawa-insa/" $ do
    conn <- liftAndCatchIO $ clone db
    -- We want the JSON body of the request, which should contain the
    -- username and password.
    req :: LoginRequest <- jsonData
    uid <- liftAndCatchIO $ searchUsers conn (T.unpack req.requestName)
    -- Try to find the user in the DB.
    user <- liftAndCatchIO $ getUser conn (head uid)
    -- Now that we've gotten the request, let's check if it's correct.
    let res = validateCredentials req ((pack . T.unpack) user.userPassword)
    -- If it isn't valid, let's give up. looks like the user sent bad data.
    -- Not worth trying to salvage, so we'll just return a 400.
    unless res (status status400 >> finish)
    -- Now that we know all is good, let's make a session for the user.
    sesh <- liftIO $ randomSession (censorUser user)
    -- Add the session to the session store, discarding the result.
    -- We don't care if it fails, at worst they'll just have to log in again.
    _ <- liftAndCatchIO $ addSession conn sesh
    -- Let's hand the session to the frontend.
    -- They can retain this for later use.
    json sesh

  post "/session/" $ do
    conn <- liftAndCatchIO $ clone db
    -- Check a user's session
    sesh :: Session <- jsonData
    let sid = sesh.sessionUUID
    -- get our copy of the session.
    sesh' <- liftAndCatchIO $ getSession conn sid
    let eq = sesh == sesh'
    json (eq, sesh')

  get "/onboard/" $ do
    conn <- liftAndCatchIO $ clone db
    -- are there any users in the database?
    -- if there are, we don't want to present the user with the onboarding page.
    res <- liftAndCatchIO $ isUsersEmpty conn
    let ok' :: IsOk = IsOk {ok = res}
    json ok'

-- | Handle search requests
-- Not sure if i'll use this, it's a bit easier to search on the client
-- using a filter/search function, but it's here if i need it.
-- It does come in handy for debugging.
searchHandler :: Connection -> ScottyM ()
searchHandler db = do
  get "/search/prods/:query" $ do
    conn <- liftAndCatchIO $ clone db
    query <- param "query"
    result <- liftIO $ try $ searchProds conn query

    -- If it's an error, return a 404. Else, return the matching data.
    case result of
      Left (e :: SomeException) -> do
        liftIO $ print e
        status status404
        finish
      Right res -> json res

-- | Purges the entire database, deleting then recreating it!
-- Be very careful with this.
purgeHandler :: Connection -> ScottyM ()
purgeHandler db =
  delete "/UNSAFE-PURGE-ALL-CHECK-FIRST-IM-SERIOUS/" $
    liftAndCatchIO $
      clone db >>= \conn ->
        liftIO (purgeDb conn)
