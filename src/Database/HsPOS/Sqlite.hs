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
import Database.HsPOS.Types ( CensoredUser (cuserId), User, Product, userName, userPassword, userPrivilege, APIError(InvalidData), DBError(NoDataError) )
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
          , "CREATE TABLE IF NOT EXISTS tills (till_id integer not null constraint tills_pk primary key autoincrement, till_name integer)"
          , "CREATE TABLE IF NOT EXISTS user_till_xref (user_id integer constraint user_till_xref_pk primary key constraint user_till_xref_users_user_id_fk references users, till_id integer constraint user_till_xref_tills_till_id_fk references tills)"
          , "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id integer constraint products_sales_xref_pk primary key references sales)"
          , "CREATE TABLE IF NOT EXISTS session (session_id text not null primary key, user_id integer references users, session_hash integer not null)"
          ]
  mapM_ (\x -> run conn x []) q
  commit conn
  disconnect conn

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
  let r'   = head r
  let r''  = head r'
  return (fromSql r'' :: Integer)

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
  let r'   = head r
  let r''  = head r'
  return (fromSql r'' :: Integer)

{- | getProdName returns an IO [String] with the product name of the
     products matching the given product ID
-}
getProdName :: FilePath -> Integer -> IO [String]
getProdName dbStr pid = do
  conn <- connectSqlite3 dbStr
  let query = "select product_name from products where product_id = (?)"
  r <- quickQuery' conn query [toSql pid]
  disconnect conn
  return (map fromSql (head r))

-- | Add a product into the database, returning True if it worked.
addProd :: FilePath -> String -> Double -> IO Bool
addProd dbStr name price = do
  when (name == "") (E.throw InvalidData)
  conn <- connectSqlite3 dbStr
  let q = "INSERT INTO products (product_name, product_price) VALUES (?, ?)"
  r <- run conn q [toSql name, toSql price]
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
  let q   = "SELECT user_id, user_name, user_privilege FROM user WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  return $ (head . map (desqlCU . tuplify3)) result

-- | Get a specific user, including the password.
getUser :: FilePath -> Integer -> IO User
getUser dbStr uid = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  when (result == [[]]) (E.throw NoDataError)
  return $ (head . map (desqlU . tuplify4)) result

isUsersEmpty :: FilePath -> IO Bool
isUsersEmpty dbStr = do
  conn <- connectSqlite3 dbStr
  let query = "SELECT COUNT(1) WHERE EXISTS (SELECT * FROM users)"
  result <- quickQuery' conn query []
  disconnect conn
  return (result == [[]])

-- | Search the list of users by a string search query.
searchUsers :: FilePath -> String -> IO [Integer]
searchUsers dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "SELECT user_id FROM users WHERE user_name LIKE (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ map (fromSql . head) result

getProd :: String -> Integer -> IO Product
getProd dbStr pid = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products WHERE product_id=(?)"
  result <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ head $ map (desqlP . tuplify3) result

-- | Returns a list of all the products in the database
allProds :: FilePath -> IO [Product]
allProds dbStr = do
  conn   <- connectSqlite3 dbStr
  let q   = "SELECT * FROM products"
  result <- quickQuery' conn q []
  disconnect conn
  return $ map (desqlP . tuplify3) result

-- | Searches the database for a product, returning a list of matching
-- | products.
searchProds :: FilePath -> String -> IO [Product]
searchProds dbStr term = do
  conn      <- connectSqlite3 dbStr
  let query  = "SELECT * FROM products WHERE product_name LIKE (?)"
  let term'  = "%" <> term <> "%"
  result    <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ map (desqlP . tuplify3) result

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
  return (desqlS (tuplify3 . head $ result) cuser)
  

addSession :: FilePath -> Session -> IO Bool
addSession dbStr sesh = do
  conn     <- connectSqlite3 dbStr
  let query = "INSERT INTO sessions (session_id, session_user, session_hash) values (?,?,?)"
  result   <- run conn query [ toSql (toString sesh.sessionUUID)
                             , toSql (cuserId sesh.sessionUser)
                             , toSql sesh.sessionHash ]
  disconnect conn
  return $ result > 0
-- SELECT till_name FROM till WHERE till.till_id=(SELECT al.till_id FROM allowed_till al where al.emp_id = (SELECT emp.emp_id FROM employees emp WHERE emp.name="example"))
