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


{-# LANGUAGE Trustworthy, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, OverloadedStrings, OverloadedRecordDot, ImportQualifiedPost #-}
{- HLINT ignore "Use camelCase" -}

module Database.HsPOS.Sqlite where
import Database.HDBC
    ( fromSql,
      toSql,
      quickQuery',
      IConnection(disconnect, run, commit) )
import Database.HDBC.Sqlite3 (connectSqlite3)
import Control.Monad (when)
import Control.Exception qualified as E
import Database.HsPOS.Types ( CensoredUser (cuserId), User, Product(productId), userName, userPassword, userPrivilege, APIError(InvalidData), DBError(NoDataError, MultipleDataError), ProductWithStock (ProductWithStock) )
import Database.HsPOS.Session
    ( Session(sessionHash, sessionUUID, sessionUser) )
import Database.HsPOS.Auth (quickHashPassword)
import Database.HsPOS.Util
    ( tuplify3, tuplify4, desqlS, desqlCU, desqlU, desqlP )
import System.Directory ( removeFile )
import Data.UUID (toString, UUID)
import Database.HDBC qualified as H
import Data.Text.Lazy qualified as T

-- SQL functions

-- | Purges the entire database
purgeDb :: FilePath -> IO ()
purgeDb dbStr = do
  removeFile dbStr
  tryCreateTables dbStr

-- | Creates the requisite tables for the database.
tryCreateTables :: FilePath -> IO ()
tryCreateTables dbStr = do
  conn <- connectSqlite3 dbStr
  let q = [ "CREATE TABLE IF NOT EXISTS users (user_id INTEGER not null primary key autoincrement, user_name TEXT not null,user_password TEXT not null, user_privilege INTEGER not null)"
          , "CREATE TABLE IF NOT EXISTS stock (product_id integer not null constraint stock_pk primary key autoincrement references products, in_stock integer)"
          , "CREATE TABLE IF NOT EXISTS sales (sales_id integer constraint sales_pk primary key autoincrement, sales_date date, number_sold integer)"
          , "CREATE TABLE IF NOT EXISTS products (product_id INTEGER not null primary key autoincrement,product_name TEXT not null,product_price DOUBLE not null)"
          , "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id integer constraint products_sales_xref_pk primary key references sales)"
          , "CREATE TABLE IF NOT EXISTS sessions (session_id text not null primary key, user_id integer references users, session_hash integer not null)"
          ]
  mapM_ (\x -> run conn x []) q
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
    []  -> ProductWithStock prod 0
    [x] -> ProductWithStock prod (fromSql $ head x)
    _ -> E.throw MultipleDataError

makeSale :: FilePath -> String -> Integer -> Integer -> IO Bool
makeSale dbStr date pid quant = do
  conn  <- connectSqlite3 dbStr
  let q1 = "INSERT INTO sales (sales_date, number_sold) VALUES (?, ?) RETURNING sales_id"
  let q2 = "INSERT INTO products_sales_xref (product_id, sales_id) values (?, ?)"
  let q3 = "UPDATE stock SET in_stock=in_stock - 1 WHERE product_id = (?) and in_stock > 0"
  sid   <- quickQuery' conn q1 [toSql date, toSql quant]
  _     <- quickQuery' conn q2 [toSql pid, (head . head) sid]
  ok    <- run conn q3 [toSql pid]
  commit conn
  disconnect conn
  return $ ok >= 1

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
    []  -> 0
    [x] -> fromSql $ head x
    _  -> E.throw MultipleDataError

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
    []  -> 0
    [x] -> fromSql $ head x
    _   -> E.throw MultipleDataError

{- | getProdName returns an IO [String] with the product name of the
     products matching the given product ID
-}
getProdName :: FilePath -> Integer -> IO [String]
getProdName dbStr pid = do
  conn <- connectSqlite3 dbStr
  let query = "SEELCT product_name FROM products WHERE product_id = (?)"
  r <- quickQuery' conn query [toSql pid]
  disconnect conn
  return (map fromSql (head r))

-- | Add a product into the database, returning True if it worked.
addProd :: FilePath -> String -> Double -> IO Bool
addProd dbStr name price = do
  when (name == "") (E.throw InvalidData)
  conn <- connectSqlite3 dbStr
  let ins :: Integer = 0
  let q  = "INSERT INTO products (product_name, product_price) VALUES (?, ?) RETURNING product_id"
  let q' = "INSERT INTO stock (product_id, in_stock) VALUES (?, ?)"
  r <- quickQuery' conn q [toSql name, toSql price]
  let pid = (head . head) r
  _ <- run conn q' [pid, toSql ins]
  commit conn
  disconnect conn
  return $ case r of
    []  -> False
    [_] -> True
    _   -> E.throw MultipleDataError

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
  r  <- run conn q [toSql usr.userName, toSql pw, toSql usr.userPrivilege]
  commit conn
  disconnect conn
  return $ r < 0

-- | Return a list of all of the users in the DB, excluding the passwords.
allCUsers :: FilePath -> IO [CensoredUser]
allCUsers dbStr = do
  conn <- connectSqlite3 dbStr
  let q = "SELECT user_id, user_name, user_privilege FROM users"
  r    <- quickQuery' conn q []
  disconnect conn
  return $ map (desqlCU . tuplify3) r

-- | Fetch a specific user, excluding the password.
getCUser :: FilePath -> Integer -> IO CensoredUser
getCUser dbStr uid = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT user_id, user_name, user_privilege FROM users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x]  -> desqlCU $ tuplify3 x
    _    -> E.throw MultipleDataError

-- | Get a specific user, including the password.
-- don't expose this through the API. duh.
getUser :: FilePath -> Integer -> IO User
getUser dbStr uid = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  when (result == [[]]) (E.throw NoDataError)
  return $ case result of
    [[]] -> E.throw NoDataError
    [x]  -> desqlU $ tuplify4 x
    _    -> E.throw MultipleDataError

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
    [x]  -> fromSql $ head x
    _    -> E.throw MultipleDataError

-- | Search the list of users by a string search query.
searchUsers :: FilePath -> String -> IO [Integer]
searchUsers dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "SELECT user_id FROM users WHERE user_name LIKE (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of
    [[]] -> []
    x    -> map (fromSql . head) x

getProd :: String -> Integer -> IO Product
getProd dbStr pid = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products WHERE product_id=(?)"
  result <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x]  -> desqlP $ tuplify3 x
    _    -> E.throw MultipleDataError

-- | Returns a list of all the products in the database
allProds :: FilePath -> IO [Product]
allProds dbStr = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> []
    x    -> map (desqlP . tuplify3) x

-- | Searches the database for a product, returning a list of matching
-- | products.
searchProds :: FilePath -> String -> IO [Product]
searchProds dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "SELECT * FROM products WHERE product_name LIKE (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of 
    [[]] -> []
    x    -> map (desqlP . tuplify3) x

getSession :: FilePath -> UUID -> IO Session
getSession dbStr sid = do
  conn     <- connectSqlite3 dbStr
  let query  = "SELECT * FROM sessions WHERE session_id = (?)"
  let query2 = "SELECT user_id FROM sessions WHERE session_id = (?)"
  result    <- H.quickQuery' conn query  [ toSql . toString $ sid ]
  result2   <- H.quickQuery' conn query2 [ toSql . toString $ sid ]
  let uid    = (fromSql . head . head) result2
  disconnect conn
  cuser     <- getCUser dbStr uid
  return $ desqlS (tuplify3 . head $ result) cuser
  

addSession :: FilePath -> Session -> IO Bool
addSession dbStr sesh = do
  conn     <- connectSqlite3 dbStr
  let query = "INSERT INTO sessions (session_id, user_id, session_hash) values (?,?,?)"
  result   <- run conn query [ toSql (toString sesh.sessionUUID)
                             , toSql (cuserId sesh.sessionUser)
                             , toSql sesh.sessionHash ]
  disconnect conn
  return $ result > 0

-- SELECT till_name FROM till WHERE till.till_id=(SELECT al.till_id FROM allowed_till al where al.emp_id = (SELECT emp.emp_id FROM employees emp WHERE emp.name="example"))
