{- 
  hs-pos
  Sqlite.hs
  Created by Lilly Cham on 8/7/22.

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

module Database.HsPOS.Internal.Sqlite where
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
import Database.HsPOS.Internal.Types
import Control.Monad  (join)
import Data.Bifunctor (bimap)

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
  let r'   = head r
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
  let r'   = head r
  let r''  = head r'
  return (fromSql r'' :: Int)


tryCreateTables :: String -> IO ()
tryCreateTables conn = do
  conn' <- connectSqlite3 conn
  let q = [ "CREATE TABLE IF NOT EXISTS users (user_id INTEGER not null primary key autoincrement, user_name TEXT not null,user_password TEXT not null, user_privilege INTEGER not null)"
          , "CREATE TABLE IF NOT EXISTS stock (product_id integer not null constraint stock_pk primary key autoincrementreferences products, in_stock integer)"
          , "CREATE TABLE IF NOT EXISTS sales (sales_id integer constraint sales_pk primary key autoincrement, sales_date date number_sold integer)"
          , "CREATE TABLE IF NOT EXISTS products (product_id INTEGER not null primary key autoincrement,product_name TEXT not null,product_price DOUBLE not null)"
          , "CREATE TABLE IF NOT EXISTS tills (till_id integer not null constraint tills_pk primary key autoincrement, till_name integer)"
          , "CREATE TABLE IF NOT EXISTS user_till_xref (user_id integer constraint user_till_xref_pk primary key constraint user_till_xref_users_user_id_fk references users, till_id integer constraint user_till_xref_tills_till_id_fk references tills)"
          , "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id   integer constraint products_sales_xref_pk primary key references sales)"
          ]

  mapM_ (\x -> H.run conn' x []) q
  disconnect conn'

-- Add a product into the database
addProd :: String -> Int -> String -> Double -> IO ()
addProd conn id name price = do
  conn' <- connectSqlite3 conn
  let q = "INSERT INTO products VALUES (?, ?, ?)"
  r <- H.quickQuery' conn' q [toSql id, toSql name, toSql price]
  disconnect conn'


sqlTuplify [id, name, pri]  = (id, name, pri)

desqlU :: (SqlValue, SqlValue, SqlValue) -> (Int, String, Int)
desqlU (x, y, z) = (fromSql x, fromSql y, fromSql z)

desqlF :: (SqlValue, SqlValue, SqlValue) -> (Int, String, Double)
desqlF (x, y, z) = (fromSql x, fromSql y, fromSql z)

-- allUsers :: String -> [(Int, String, Int)]
allUsers conn = do
  conn' <- connectSqlite3 conn
  let q = "SELECT user_id, user_name, user_privilege FROM users"
  r <- H.quickQuery' conn' q []
  return $ map desqlU $ map sqlTuplify r

-- allUsers :: String -> [(Int, String, Int)]
allProds conn = do
  conn' <- connectSqlite3 conn
  let q = "SELECT * FROM products"
  r <- H.quickQuery' conn' q []
  return $ map desqlF $ map sqlTuplify r

-- users = Unsafe.unsafePerformIO (getUsers "store.db")

-- SELECT till_name FROM till WHERE till.till_id=(SELECT al.till_id FROM allowed_till al where al.emp_id = (SELECT emp.emp_id FROM employees emp WHERE emp.name="example"))
