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
import Control.Monad (join, when, unless)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HsPOS.Internal.Types
import Data.Tuple.Curry (uncurryN)
import qualified Data.Text.Lazy as T

-- SQL functions

-- | Creates the requisite tables for the database.
tryCreateTables :: FilePath -> IO ()
tryCreateTables dbStr = do
  conn <- connectSqlite3 dbStr
  let q = [ "CREATE TABLE IF NOT EXISTS users (user_id INTEGER not null primary key autoincrement, user_name TEXT not null,user_password TEXT not null, user_privilege INTEGER not null)"
          , "CREATE TABLE IF NOT EXISTS stock (product_id integer not null constraint stock_pk primary key autoincrement references products, in_stock integer)"
          , "CREATE TABLE IF NOT EXISTS sales (sales_id integer constraint sales_pk primary key autoincrement, sales_date date number_sold integer)"
          , "CREATE TABLE IF NOT EXISTS products (product_id INTEGER not null primary key autoincrement,product_name TEXT not null,product_price DOUBLE not null)"
          , "CREATE TABLE IF NOT EXISTS tills (till_id integer not null constraint tills_pk primary key autoincrement, till_name integer)"
          , "CREATE TABLE IF NOT EXISTS user_till_xref (user_id integer constraint user_till_xref_pk primary key constraint user_till_xref_users_user_id_fk references users, till_id integer constraint user_till_xref_tills_till_id_fk references tills)"
          , "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id integer constraint products_sales_xref_pk primary key references sales)"
          ]
  x <- mapM (\x -> run conn x []) q
  commit conn
  disconnect conn

makeSale :: String -> String -> Int-> Int -> IO Bool
makeSale dbStr date id quant = do
  conn  <- connectSqlite3 dbStr
  let q1 = "INSERT INTO sales (sales_date, number_sold) VALUES (?, ?) RETURNING sales_id"
  let q2 = "INSERT INTO products_sales_xref (product_id, sales_id) values (?, ?)"
  let q3 = "UPDATE stock SET in_stock=in_stock - 1 WHERE product_id = (?) and in_stock > 0"
  sid   <- quickQuery' conn q1 [toSql date, toSql quant]
  quickQuery' conn q2 [toSql id, (head . head) sid]
  ok    <- run conn q3 [toSql id]
  commit conn
  disconnect conn
  return $ ok >= 1

-- | monthlySales takes:
-- dbStr, a filepath to the database,
-- d, a date in the format "YYYY-MM",
-- id, a product id, 
-- and returns the total sales for that month.
monthlyTotalSales :: FilePath -> String -> Int -> IO Int
monthlyTotalSales dbStr d id = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql id]
  disconnect conn
  let r'   = head r
  let r''  = head r'
  return (fromSql r'' :: Int)

monthlySales :: FilePath -> String -> Int -> IO [Int]
monthlySales dbStr d id = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql id]
  disconnect conn
  return $ concat (map (map fromSql) r)

rangeSales :: FilePath -> String -> String -> Int -> IO [[Int]]
rangeSales dbStr date date2 id = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) BETWEEN (?) AND (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql date, toSql date2, toSql id]
  disconnect conn
  return $ map (map fromSql) r

-- | yearlySales returns the total sales of said product id for that year.
yearlySales :: FilePath -> String -> Int -> IO Int
yearlySales dbStr d id = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql id]
  disconnect conn
  let r'   = head r
  let r''  = head r'
  return (fromSql r'' :: Int)

{- | getProdName returns an IO [String] with the product name of the
     products matching the given product ID
-}
getProdName :: FilePath -> Int -> IO [String]
getProdName dbStr id = do
  conn <- connectSqlite3 dbStr
  let query = "select product_name from products where product_id = (?)"
  r <- quickQuery' conn query [toSql id]
  disconnect conn
  return (map fromSql (head r))

-- | Add a product into the database, returning True if it worked.
addProd :: String -> String -> Double -> IO Bool
addProd dbStr name price = do
  conn <- connectSqlite3 dbStr
  let q = "INSERT INTO products (product_name, product_price) VALUES (?, ?)"
  r <- quickQuery' conn q [toSql name, toSql price]
  commit conn
  disconnect conn
  return $ if ((length $ head r) >= 0) then False else True

tuplify3 [x, y, z] = (x, y, z)

{- | "DeSQL" a censored user (Int,Text,Int),
     ie. turn it from SQL types into native haskell types. -}
desqlCU :: (SqlValue, SqlValue, SqlValue) -> CensoredUser
desqlCU (id, name, priv) =
  CensoredUser {cuser_id = (fromSql id)
               , cuser_name = (T.pack $ fromSql name)
               , cuser_privilege = (fromSql priv)
               }

{- | "DeSQL" a product (or any other value in the form (Int,String,Double),
     ie. turn it from SQL types into native haskell types. -}
desqlF :: (SqlValue, SqlValue, SqlValue) -> (Int, String, Double)
desqlF (x, y, z) = (fromSql x, fromSql y, fromSql z)

allCUsers :: FilePath -> IO [CensoredUser]
allCUsers dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT user_id, user_name, user_privilege FROM users"
  r    <- quickQuery' conn q []
  disconnect conn
  return $ map desqlCU $ map tuplify3 r

getCUser :: String -> Int -> IO CensoredUser --(Int, String, Int)
getCUser dbStr id = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM user where user_id=(?)"
  result <- quickQuery' conn q [toSql id]
  disconnect conn
  return $ (head . map desqlCU . map tuplify3) result

searchUsers :: FilePath -> String -> IO [[Int]]
searchUsers dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "select user_id from products where user_name like (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ map (\x -> map fromSql x) result

getProd :: String -> Int -> IO (Int, String, Double)
getProd dbStr id = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products where product_id=(?)"
  result <- quickQuery' conn q [toSql id]
  disconnect conn
  return $ head $ map desqlF $ map tuplify3 result

-- allUsers :: String -> [(Int, String, Int)]
allProds dbStr = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products"
  result <- quickQuery' conn q []
  disconnect conn
  return $ map desqlF $ map tuplify3 result

searchProds :: FilePath -> String -> IO [[Int]]
searchProds dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "select product_id from products where product_name like (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ map (\x -> map fromSql x) result

-- SELECT till_name FROM till WHERE till.till_id=(SELECT al.till_id FROM allowed_till al where al.emp_id = (SELECT emp.emp_id FROM employees emp WHERE emp.name="example"))
