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


{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , MultiParamTypeClasses
           , ScopedTypeVariables 
           , StandaloneDeriving 
           , TypeFamilies
           , TypeSynonymInstances 
           , OverloadedStrings
           , TypeApplications
           , OverloadedLabels
#-}


module Main where      
import Control.Applicative
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson ((.=))
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as T
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Data.Aeson as A
import qualified Options.Applicative as Opt
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
import Database.HDBC
import qualified Database.HDBC as H
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified System.IO.Unsafe as Unsafe
import Data.IORef


data Args = Args
  { optDb     :: String
  , optPort   :: Int
  , optQuiet  :: Bool
  }


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


main :: IO ()
main = do
  opts <- execParser opts
  let port = optPort opts

  -- withSQLite (optDb opts) $ do
  --   tryCreateTable users
  --   tryCreateTable products
  --   usersSelect <- query $ do
  --     user <- select users
  --     return (user ! #user_name :*: user ! #user_privilege)
  --   liftIO $ print usersSelect

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public") 
    if optQuiet opts /= True
      then middleware logStdoutDev
      else return ()

    home >> adm >> restHandle >> saleHandle
  
  where
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Run a server for an EPOS system, with a REST API and Database."
      <> Opt.header "hs-pos -- A Haskell EPOS backend"
      )


-- Connection handlers


home :: ScottyM ()
home = get "/" $ file "./public/index.html"


adm :: ScottyM ()
adm = get "/dashboard" $ file "./public/dash.html"


restHandle :: ScottyM ()
restHandle = do 
  get "/users/:u" $ do
    json $ A.toJSON exampleUser
  post "/users/:u" $ do
    html "post"
  delete "/users/:u" $ do 
    html "delete"
  put "/users/:u" $ do
    html "put"
  
  get "/prods/:u" $ do
    html "get"
  post "/prods/:u" $ do
    html "post"
  delete "/prods/:u" $ do 
    html "delete"
  put "/prods/:u" $ do
    html "put"



saleHandle :: ScottyM ()
saleHandle = do
  get "/sales/:date/:id/" $ do 
    id <- param "id"
    date :: String <- param "date"
    let a = Unsafe.unsafePerformIO (monthlySales "store.db" date id)
    text $ T.pack $ show a
    
getSqlVal = Unsafe.unsafePerformIO $ newIORef 10
getMo = Unsafe.unsafePerformIO (monthlySales "store.db" "2022-07" 0)


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

exampleUser = User 1 "admin" "password" 1

-- users :: Table User
-- users = table "users" [#user_id :- primary]

-- products :: Table Product
-- products = table "products" [#product_id :- primary]


-- SQL functions


-- | monthlySales takes:
-- conn, a connection,
-- d, a date in the format "YYYY-MM",
-- id, a product id, 
-- and returns the total sales for that month.
monthlySales :: String -> String -> Int -> IO Int
monthlySales conn d id = do
  conn' <- connectSqlite3 conn
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- H.quickQuery' conn' q [toSql d, toSql id]
  disconnect conn'
  let r'   = r !! 0
  let r''  = head r'
  return (fromSql r'' :: Int)

-- | yearlySales takes:
-- conn, a connection,
-- d, a date in the format "YYYY",
-- id, a product id, 
-- and returns the total sales of said product id for that year.
yearlySales :: String -> String -> Int -> IO Int
yearlySales conn d id = do
  conn' <- connectSqlite3 conn
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- H.quickQuery' conn' q [toSql d, toSql id]
  disconnect conn'
  let r'   = r !! 0
  let r''  = head r'
  return (fromSql r'' :: Int)


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
