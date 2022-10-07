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
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- HLINT ignore "Use camelCase" -}

-- | This module contains Haskell interfaces to SQL queries for the SQLite database.
module Database.HsPOS.Sqlite where

import Control.Exception qualified as E
import Control.Monad (when)
import Data.Text.Lazy qualified as T
import Data.UUID (UUID, toString)
import Database.HDBC
  ( IConnection (commit, disconnect, run),
    fromSql,
    quickQuery',
    toSql,
  )
import Database.HDBC qualified as H
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HsPOS.Auth (quickHashPassword)
import Database.HsPOS.Session
  ( Session (sessionHash, sessionUUID, sessionUser),
  )
import Database.HsPOS.Types (APIError (InvalidData), CensoredUser (cuserId), DBError (MultipleDataError, NoDataError), Product (productId), ProductWithStock (ProductWithStock), User, userName, userPassword, userPrivilege)
import Database.HsPOS.Util
  ( desqlCU,
    desqlP,
    desqlPS,
    desqlS,
    desqlU,
    tuplify3,
    tuplify4,
  )
import System.Directory (removeFile)

-- SQL functions

-- | Purges the entire database
purgeDb :: FilePath -> IO ()
purgeDb dbStr = do
  removeFile dbStr
  tryCreateTables dbStr

-- | Creates several convenience views to simplify queries
mkViews :: FilePath -> IO ()
mkViews dbStr = do
  conn <- connectSqlite3 dbStr
  let views =
        [ "create view if not exists products_sales_by_date as select products.product_name , products.product_id , sales_date as month , sum(s.number_sold) as total_sold from products left join products_sales_xref psx on products.product_id = psx.product_id left join sales s on psx.sales_id = s.sales_id group by products.product_id, month",
          "create view if not exists censored_users as select user_id , user_name , user_privilege from users",
          "create view if not exists top_products as select p.product_name , p.product_id , sum(s.number_sold) as total_sold from products as p left join products_sales_xref psx on p.product_id = psx.product_id left join sales as s on psx.sales_id = s.sales_id group by p.product_id order by total_sold desc",
          "create view if not exists best_product as select p.product_name , p.product_id , p.product_price , total_sold from top_products as t_p left join products p on t_p.product_id = p.product_id where total_sold=(select max(total_sold) from top_products)",
          "create view if not exists worst_product as select p.product_name , p.product_id , p.product_price , total_sold from top_products as t_p left join products p on t_p.product_id = p.product_id where total_sold=(select min(total_sold) from top_products)"
        ]
  -- run every sql query above ^
  -- mapM_ is like a map but it only runs for effects.
  mapM_ (\x -> run conn x []) views
  -- Commit the changes to the database (prevents db locking errors)
  commit conn
  -- Drop the connection
  disconnect conn

-- | Creates the requisite tables for the database.
tryCreateTables :: FilePath -> IO ()
tryCreateTables dbStr = do
  conn <- connectSqlite3 dbStr
  let q =
        [ "CREATE TABLE IF NOT EXISTS users (user_id INTEGER not null primary key autoincrement, user_name TEXT not null,user_password TEXT not null, user_privilege INTEGER not null)",
          "CREATE TABLE IF NOT EXISTS stock (product_id integer not null constraint stock_pk primary key autoincrement references products, in_stock integer)",
          "CREATE TABLE IF NOT EXISTS sales (sales_id integer constraint sales_pk primary key autoincrement, sales_date date, number_sold integer)",
          "CREATE TABLE IF NOT EXISTS products (product_id INTEGER not null primary key autoincrement,product_name TEXT not null,product_price DOUBLE not null)",
          "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id integer constraint products_sales_xref_pk primary key references sales)",
          "CREATE TABLE IF NOT EXISTS sessions (session_id text not null primary key, user_id integer references users, session_hash integer not null)"
        ]
  mapM_ (\x -> run conn x []) q
  commit conn
  mkViews dbStr
  commit conn
  disconnect conn

-- | Sets stock of `pid` to `num`
setStock :: FilePath -> Integer -> Integer -> IO Bool
setStock dbStr pid num = do
  conn <- connectSqlite3 dbStr
  let q = "UPDATE stock SET in_stock=(?) WHERE product_id = (?)"
  ok <- run conn q [toSql num, toSql pid]
  commit conn
  disconnect conn
  return $ ok >= 1

-- | Gets the stock of a Product. Returns a ProductWithStock
getInStock :: FilePath -> Product -> IO ProductWithStock
getInStock dbStr prod = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT in_stock FROM stock WHERE product_id = (?)"
  result <- quickQuery' conn q [toSql prod.productId]
  disconnect conn
  return $ case result of
    [] -> ProductWithStock prod 0
    [x] -> ProductWithStock prod (fromSql $ head x)
    _ -> E.throw MultipleDataError

-- | Records a sale of `quant` of `pid` on `date`.
makeSale :: FilePath -> String -> Integer -> Integer -> IO Bool
makeSale dbStr date pid quant = do
  conn <- connectSqlite3 dbStr
  let q1 = "INSERT INTO sales (sales_date, number_sold) VALUES (?, ?) RETURNING sales_id"
  let q2 = "INSERT INTO products_sales_xref (product_id, sales_id) values (?, ?)"
  let q3 = "UPDATE stock SET in_stock=in_stock - 1 WHERE product_id = (?) and in_stock > 0"
  let q4 = "INSERT INTO out_of_stock (product_id, oos_date) values (?, ?)"
  sid <- quickQuery' conn q1 [toSql date, toSql quant]
  _ <- quickQuery' conn q2 [toSql pid, (head . head) sid]
  ok <- run conn q3 [toSql pid]
  commit conn
  disconnect conn
  return $ ok >= 1

percentSalesDiff :: FilePath -> IO (Double, Bool)
percentSalesDiff dbStr = do
  conn <- connectSqlite3 dbStr
  let q1 = "SELECT coalesce(sum(number_sold), 0) FROM sales WHERE sales_date >= date('now', '-1 month')"
  let q2 = "SELECT coalesce(sum(number_sold), 0) FROM sales WHERE sales_date >= date('now', '-2 month') AND sales_date < date('now', '-1 month')"
  result <- quickQuery' conn q1 []
  result2 <- quickQuery' conn q2 []
  disconnect conn
  case result of
    [] -> E.throw NoDataError
    [x] -> case result2 of
      [] -> E.throw NoDataError
      -- Percent difference between two months
      [y] ->
        let x' = fromSql $ head x
            y' = fromSql $ head y
            absDiff = abs (x' - y')
            avg = (x' + y') / 2
            up = x' > y'
         in return (absDiff / avg, up)
      _ -> E.throw MultipleDataError
    _ -> E.throw MultipleDataError

-- | monthlySales takes:
-- dbStr, a filepath to the database,
-- d, a date in the format "YYYY-MM",
-- id, a product id,
-- and returns the total sales for that month.
monthlyTotalSales :: FilePath -> String -> Integer -> IO Integer
monthlyTotalSales dbStr d pid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ case r of
    [] -> 0
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

monthlySales :: FilePath -> String -> Integer -> IO [Integer]
monthlySales dbStr d pid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ concatMap (map fromSql) r

rangeSales :: FilePath -> String -> String -> Integer -> IO [[Integer]]
rangeSales dbStr date date2 pid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) BETWEEN (?) AND (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql date, toSql date2, toSql pid]
  disconnect conn
  return $ map (map fromSql) r

-- | yearlySales returns the total sales of said product id for that year.
yearlySales :: FilePath -> String -> Integer -> IO Integer
yearlySales dbStr d pid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ case r of
    [] -> 0
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

-- | getProdName returns an IO [String] with the product name of the
--     products matching the given product ID
getProdName :: FilePath -> Integer -> IO [String]
getProdName dbStr pid = do
  conn <- connectSqlite3 dbStr
  let query = "SEELCT product_name FROM products WHERE product_id = (?)"
  r <- quickQuery' conn query [toSql pid]
  disconnect conn
  return (map fromSql (head r))

getProdWithStock :: FilePath -> Integer -> IO ProductWithStock
getProdWithStock dbStr pid = do
  conn <- connectSqlite3 dbStr
  let q = "select p.*, s.in_stock from products as p left join stock s on p.product_id = s.product_id left join products_sales_xref psx on p.product_id = psx.product_id where p.product_id=(?)"
  r <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ case r of
    [[]] -> E.throw NoDataError
    [x] -> desqlPS . tuplify4 $ x
    _ -> E.throw MultipleDataError

-- | getProdWithStocks returns all products, but tagged with their stock info.
getProdsWithStock :: FilePath -> IO [ProductWithStock]
getProdsWithStock dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "select p.*, s.in_stock from products as p left join stock s on p.product_id = s.product_id left join products_sales_xref psx on p.product_id = psx.product_id"
  r <- quickQuery' conn q []
  disconnect conn
  return $ case r of
    [[]] -> E.throw NoDataError
    x -> map (desqlPS . tuplify4) x

-- | Add a product into the database, returning True if it worked.
addProd :: FilePath -> String -> Double -> IO Bool
addProd dbStr name price = do
  when (name == "") (E.throw InvalidData)
  conn <- connectSqlite3 dbStr
  let ins :: Integer = 0
  let q = "INSERT INTO products (product_name, product_price) VALUES (?, ?) RETURNING product_id"
  let q' = "INSERT INTO stock (product_id, in_stock) VALUES (?, ?)"
  r <- quickQuery' conn q [toSql name, toSql price]
  let pid = (head . head) r
  _ <- run conn q' [pid, toSql ins]
  commit conn
  disconnect conn
  return $ case r of
    [] -> False
    [_] -> True
    _ -> E.throw MultipleDataError

-- | Remove product `pid` from the database
removeProd :: FilePath -> Integer -> IO Bool
removeProd dbStr pid = do
  conn <- connectSqlite3 dbStr
  let q = "DELETE FROM products WHERE product_id=(?)"
  r <- run conn q [toSql pid]
  commit conn
  disconnect conn
  return $ r < 0

-- | Add a User into the database, returning True if it worked.
addUser :: FilePath -> User -> IO Bool
addUser dbStr usr = do
  conn <- connectSqlite3 dbStr
  let q = "INSERT INTO users (user_name, user_password, user_privilege) VALUES (?, ?, ?)"
  pw <- quickHashPassword $ T.unpack usr.userPassword
  r <- run conn q [toSql usr.userName, toSql pw, toSql usr.userPrivilege]
  commit conn
  disconnect conn
  return $ r < 0

-- | Return a list of all of the users in the DB, excluding the passwords.
allCUsers :: FilePath -> IO [CensoredUser]
allCUsers dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT user_id, user_name, user_privilege FROM users"
  r <- quickQuery' conn q []
  disconnect conn
  return $ map (desqlCU . tuplify3) r

-- | Fetch a specific user, excluding the password.
getCUser :: FilePath -> Integer -> IO CensoredUser
getCUser dbStr uid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT * FROM censored_users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlCU $ tuplify3 x
    _ -> E.throw MultipleDataError

-- | Get a specific user, including the password.
-- don't expose this through the API. duh.
getUser :: FilePath -> Integer -> IO User
getUser dbStr uid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT * FROM users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  when (result == [[]]) (E.throw NoDataError)
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlU $ tuplify4 x
    _ -> E.throw MultipleDataError

-- | Remove a user from the database.
removeUser :: FilePath -> Integer -> IO Bool
removeUser dbStr uid = do
  conn <- connectSqlite3 dbStr
  let q = "DELETE FROM users WHERE user_id=(?)"
  r <- run conn q [toSql uid]
  commit conn
  disconnect conn
  return $ r < 0

-- | Test if there's anything in the users table, returning True if there is.
isUsersEmpty :: FilePath -> IO Bool
isUsersEmpty dbStr = do
  conn <- connectSqlite3 dbStr
  let query = "SELECT COUNT(1) WHERE EXISTS (SELECT * FROM users)"
  result <- quickQuery' conn query []
  disconnect conn
  return $ case result of
    [[]] -> True
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

-- | Search the list of users by a string search query.
searchUsers :: FilePath -> String -> IO [Integer]
searchUsers dbStr term = do
  conn <- connectSqlite3 dbStr
  let query = "SELECT user_id FROM users WHERE user_name LIKE (?)"
  let term' = "%" <> term <> "%"
  result <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of
    [[]] -> []
    x -> map (fromSql . head) x

getProd :: String -> Integer -> IO Product
getProd dbStr pid = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT * FROM products WHERE product_id=(?)"
  result <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> E.throw MultipleDataError

-- | Returns a list of all the products in the database
allProds :: FilePath -> IO [Product]
allProds dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT * FROM products"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> []
    x -> map (desqlP . tuplify3) x

-- | Returns the most sold product in the database.
topProd :: FilePath -> IO Product
topProd dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "select product_id,product_name,product_price from best_product"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> head $ map (desqlP . tuplify3) result

-- | Returns the least sold product in the database.
bottomProd :: FilePath -> IO Product
bottomProd dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "select product_id,product_name,product_price from worst_product"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> head $ map (desqlP . tuplify3) result

-- | Searches the database for a product, returning a list of matching
-- | products.
searchProds :: FilePath -> String -> IO [Product]
searchProds dbStr term = do
  conn <- connectSqlite3 dbStr
  let query = "SELECT * FROM products WHERE product_name LIKE (?)"
  let term' = "%" <> term <> "%"
  result <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError -- handle this on caller side, throw a 404
    x -> map (desqlP . tuplify3) x

getSession :: FilePath -> UUID -> IO Session
getSession dbStr sid = do
  conn <- connectSqlite3 dbStr
  let query = "SELECT * FROM sessions WHERE session_id = (?)"
  let query2 = "SELECT user_id FROM sessions WHERE session_id = (?)"
  result <- H.quickQuery' conn query [toSql . toString $ sid]
  result2 <- H.quickQuery' conn query2 [toSql . toString $ sid]
  let uid = (fromSql . head . head) result2
  disconnect conn
  cuser <- getCUser dbStr uid
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlS (tuplify3 x) cuser
    _ -> E.throw MultipleDataError

addSession :: FilePath -> Session -> IO Bool
addSession dbStr sesh = do
  conn <- connectSqlite3 dbStr
  let query = "INSERT INTO sessions (session_id, user_id, session_hash) values (?,?,?)"
  result <-
    run
      conn
      query
      [ toSql (toString sesh.sessionUUID),
        toSql (cuserId sesh.sessionUser),
        toSql sesh.sessionHash
      ]
  disconnect conn
  return $ result > 0

-- SELECT till_name FROM till WHERE till.till_id=(SELECT al.till_id FROM allowed_till al where al.emp_id = (SELECT emp.emp_id FROM employees emp WHERE emp.name="example"))
