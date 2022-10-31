{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Sqlite
-- License: BSD3
-- Stability: Unstable
-- Portability: GHC
-- Description: Haskell interfaces to SQL queries for the SQLite database.
-- This module contains Haskell interfaces to SQL queries for the SQLite database.
module Database.HsPOS.Sqlite where

import Control.Exception qualified as E
import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Data.Text.Lazy qualified as T
-- import Data.Time.Calendar.OrdinalDate (Day)

-- import Data.Vector (fromList)

-- import Database.HsPOS.Math (leastSq)

import Data.Time (diffDays)
import Data.Time.Calendar (Day)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), iso8601ParseM)
import Data.UUID (UUID, toString)
import Data.Vector (fromList)
import Database.HDBC
  ( IConnection (commit, disconnect, run),
    fromSql,
    quickQuery',
    toSql,
  )
import Database.HDBC qualified as H
import Database.HDBC.PostgreSQL (Connection)
import Database.HDBC.Types (clone)
import Database.HsPOS.Auth (quickHashPassword)
import Database.HsPOS.Math (absDiff, average, leastSq)
import Database.HsPOS.Session (Session (sessionHash, sessionUUID, sessionUser))
import Database.HsPOS.Types
  ( APIError (InvalidData),
    CensoredUser (cuserId),
    DBError (MultipleDataError, NoDataError),
    Product (productId),
    ProductWithStock (ProductWithStock),
    User,
    userName,
    userPassword,
    userPrivilege,
  )
import Database.HsPOS.Util
  ( desqlCU,
    desqlP,
    desqlPS,
    desqlS,
    desqlU,
    tuplify3,
    tuplify4,
  )

-- SQL functions

-- | Purges the entire database
purgeDb :: Connection -> IO ()
purgeDb db = do
  run db "DROP A" []
  return ()

-- | Creates several convenience views to simplify queries
mkViews :: Connection -> IO ()
mkViews db = do
  conn <- clone db
  let views =
        [ "CREATE OR REPLACE VIEW products_sales_by_date as select products.product_name , products.product_id , sales_date as month , sum(s.number_sold) as total_sold from products left join products_sales_xref psx on products.product_id = psx.product_id left join sales s on psx.sales_id = s.sales_id group by products.product_id, month",
          "CREATE OR REPLACE VIEW censored_users as select user_id , user_name , user_privilege from users",
          "CREATE OR REPLACE VIEW top_products as select p.product_name , p.product_id , sum(s.number_sold) as total_sold from products as p left join products_sales_xref psx on p.product_id = psx.product_id left join sales as s on psx.sales_id = s.sales_id group by p.product_id order by total_sold desc",
          "CREATE OR REPLACE VIEW best_product as select p.product_name , p.product_id , p.product_price , total_sold from top_products as t_p left join products p on t_p.product_id = p.product_id where total_sold=(select max(total_sold) from top_products)",
          "CREATE OR REPLACE VIEW worst_product as select p.product_name , p.product_id , p.product_price , total_sold from top_products as t_p left join products p on t_p.product_id = p.product_id where total_sold=(select min(total_sold) from top_products)"
        ]
  -- run every sql query above ^
  -- mapM_ is like a map but it only runs for effects.
  mapM_ (\x -> run conn x []) views
  -- Commit the changes to the database (prevents db locking errors)
  commit conn
  -- Drop the connection
  disconnect conn

-- | Creates the requisite tables for the database.
tryCreateTables :: Connection -> IO ()
tryCreateTables db = do
  conn <- clone db
  let q =
        [ "CREATE TABLE IF NOT EXISTS users (user_id serial not null primary key, user_name TEXT not null,user_password TEXT not null, user_privilege INTEGER not null)",
          "CREATE TABLE IF NOT EXISTS sessions (session_id text not null primary key, user_id integer references users, session_hash integer not null)",
          "CREATE TABLE IF NOT EXISTS products (product_id serial not null primary key,product_name TEXT not null,product_price DOUBLE not null)",
          "CREATE TABLE IF NOT EXISTS sales (sales_id serial constraint sales_pk primary key, sales_date date, number_sold integer)",
          "CREATE TABLE IF NOT EXISTS products_sales_xref (product_id integer references products, sales_id integer constraint products_sales_xref_pk primary key references sales)",
          "CREATE TABLE IF NOT EXISTS stock (product_id serial not null constraint stock_pk primary key references products, in_stock integer)",
          "CREATE TABLE IF NOT EXISTS restocks(restock_id serial PRIMARY key NOT NULL, restock_date DATE NOT NULL, restock_qty INTEGER NOT NULL)",
          "CREATE TABLE IF NOT EXISTS out_of_stock( oos_id serial PRIMARY key NOT NULL, oos_date DATE NOT NULL)",
          "CREATE TABLE IF NOT EXISTS restock_stock_xref( restock_id serial NOT NULL, stock_id INTEGER NOT NULL, FOREIGN KEY(restock_id) REFERENCES restocks(restock_id), FOREIGN KEY(stock_id) REFERENCES stock(stock_id), CONSTRAINT rsx_id PRIMARY KEY(restock_id, stock_id))",
          "CREATE TABLE IF NOT EXISTS oos_products_xref( oos_id INTEGER NOT NULL, product_id INTEGER NOT NULL, FOREIGN KEY(oos_id) REFERENCES out_of_stock(oos_id), FOREIGN KEY(product_id) REFERENCES products(product_id), CONSTRAINT opx_id PRIMARY KEY (oos_id, product_id))",
          "CREATE TABLE IF NOT EXISTS oos_stock_xref( oos_id INTEGER NOT NULL, stock_id INTEGER NOT NULL, FOREIGN KEY(oos_id) REFERENCES out_of_stock(oos_id), FOREIGN KEY(stock_id) REFERENCES stock(stock_id), CONSTRAINT osx_id PRIMARY KEY (oos_id, stock_id))"
        ]
  mapM_ (\x -> run conn x []) q
  commit conn
  mkViews db
  commit conn
  disconnect conn

-- | Sets stock of `pid` to `num`
setStock :: Connection -> Integer -> Integer -> IO Bool
setStock db pid num = do
  conn <- clone db
  let q1 = "SELECT in_stock FROM stock WHERE product_id = ?"
  let q2 = "UPDATE stock SET in_stock = ? WHERE product_id = ?"
  let q3 = "INSERT INTO restocks (restock_date, restock_qty) VALUES (date('now'), ?) returning restock_id"
  let q4 = "INSERT INTO restock_stock_xref (restock_id, stock_id) VALUES (?, ?)"
  cur <- quickQuery' conn q1 [toSql pid]
  let sid = case cur of
        [] -> Nothing
        _ -> Just (head (head cur))
  ok <- run conn q2 [toSql num, toSql pid]
  rid <- quickQuery' conn q3 [toSql num]
  let rid' = case rid of
        [] -> Nothing
        _ -> Just (head (head rid))
  when
    (isJust sid && isJust rid')
    ( do
        _ <- run conn q4 [fromJust rid', toSql pid]
        pure ()
    )

  commit conn
  disconnect conn
  return $ ok >= 1

-- | Gets the stock of a Product. Returns a ProductWithStock
getInStock :: Connection -> Product -> IO ProductWithStock
getInStock db prod = do
  conn <- clone db
  let q = "SELECT in_stock FROM stock WHERE product_id = (?)"
  result <- quickQuery' conn q [toSql prod.productId]
  disconnect conn
  return $ case result of
    [] -> ProductWithStock prod 0
    [x] -> ProductWithStock prod (fromSql $ head x)
    _ -> E.throw MultipleDataError

leastSqProd :: Connection -> Integer -> Integer -> IO Double
leastSqProd db pid num = do
  conn <- clone db
  let q1 = "SELECT restock_date FROM restocks r LEFT JOIN stock s LEFT JOIN restock_stock_xref rsx ON (rsx.stock_id = s.stock_id and rsx.restock_id = r.restock_id) LEFT JOIN products p ON p.product_id = s.product_id WHERE p.product_id = (?)"
  let q2 = "SELECT restock_qty FROM restocks r LEFT JOIN stock s LEFT JOIN restock_stock_xref rsx ON (rsx.stock_id = s.stock_id and rsx.restock_id = r.restock_id) WHERE product_id = (?)"
  let q3 = "SELECT oos_date FROM out_of_stock oos LEFT JOIN products p LEFT JOIN oos_products_xref opx ON (opx.product_id = p.product_id and opx.oos_id = oos.oos_id) WHERE p.product_id = (?)"
  restocks <- quickQuery' conn q1 [toSql pid]
  restockQty <- quickQuery' conn q2 [toSql pid]
  let restockQty' :: [Double] = map (fromSql . head) restockQty
  oos <- quickQuery' conn q3 [toSql pid]
  restocks' :: [Day] <- do
    let x :: [String] = concatMap (map fromSql) restocks
    mapM iso8601ParseM x
  oos' :: [Day] <- do
    let x :: [String] = concatMap (map fromSql) oos
    mapM iso8601ParseM x
  let dateDiffs :: [Double] = map fromIntegral $ zipWith diffDays restocks' oos'
  let res = leastSq (fromList dateDiffs) (fromList restockQty') (fromIntegral num)
  print res
  disconnect conn
  return res

-- | Records a sale of `quant` of `pid` on `date`.
makeSale :: Connection -> String -> Integer -> Integer -> IO Bool
makeSale db date pid quant = do
  conn <- clone db
  let q1 = "INSERT INTO sales (sales_date, number_sold) VALUES (?, ?) RETURNING sales_id"
  let q2 = "INSERT INTO products_sales_xref (product_id, sales_id) values (?, ?)"
  let q3 = "UPDATE stock SET in_stock=in_stock - 1 WHERE product_id = (?) and in_stock > -1"
  let q4 = "SELECT in_stock FROM stock WHERE product_id = (?)"
  let q5 = "INSERT INTO out_of_stock (oos_date) values (?) returning oos_id"
  let q6 = "INSERT INTO oos_products_xref (oos_id, product_id) values (?, ?)"
  sid <- quickQuery' conn q1 [toSql date, toSql quant]
  _ <- quickQuery' conn q2 [toSql pid, (head . head) sid]
  ok <- run conn q3 [toSql pid]
  oos <- quickQuery' conn q4 [toSql pid]
  when (oos == [[toSql (0 :: Integer)]]) $ do
    oid <- quickQuery' conn q5 [toSql date]
    _ <- quickQuery' conn q6 [toSql pid, (head . head) oid]
    return ()
  commit conn
  disconnect conn
  return $ ok >= 1

percentSalesDiff :: Connection -> IO (Double, Bool)
percentSalesDiff db = do
  conn <- clone db
  let q1 = "SELECT coalesce(sum(number_sold), 0) FROM sales WHERE sales_date >= (select current_date) - interval '1' month"
  let q2 = "SELECT coalesce(sum(number_sold), 0) FROM sales WHERE sales_date >= (select current_date) - interval '2' month AND sales_date >= (select current_date) - interval '1' month"
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
            ad = absDiff x' y'
            avg = average $ fromList [x', y']
            up = x' > y' -- Is the first month's sales greater than the second?
         in return (ad / avg, up)
      _ -> E.throw MultipleDataError
    _ -> E.throw MultipleDataError

-- | monthlySales takes:
-- db, a Connection to the database,
-- d, a date in the format "YYYY-MM",
-- id, a product id,
-- and returns the total sales for that month.
monthlyTotalSales :: Connection -> String -> Integer -> IO Integer
monthlyTotalSales db d pid = do
  conn <- clone db
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE to_char(sales_date, '%Y-%m') = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ case r of
    [] -> 0
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

monthlySales :: Connection -> String -> Integer -> IO [Integer]
monthlySales db d pid = do
  conn <- clone db
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE to_char(sales_date, '%Y-%m') = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ concatMap (map fromSql) r

rangeSales :: Connection -> String -> String -> Integer -> IO [[Integer]]
rangeSales db date date2 pid = do
  conn <- clone db
  let q = "SELECT number_sold FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE to_char(sales_date, '%Y-%m') BETWEEN (?) AND (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql date, toSql date2, toSql pid]
  disconnect conn
  return $ map (map fromSql) r

-- | yearlySales returns the total sales of said product id for that year.
yearlySales :: Connection -> String -> Integer -> IO Integer
yearlySales db d pid = do
  conn <- clone db
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE to_char(sales_date, '%Y') = (?) AND products_sales_xref.product_id = (?)"
  r <- quickQuery' conn q [toSql d, toSql pid]
  disconnect conn
  return $ case r of
    [] -> 0
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

-- | getProdName returns an IO [String] with the product name of the
--     products matching the given product ID
getProdName :: Connection -> Integer -> IO [String]
getProdName db pid = do
  conn <- clone db
  let query = "SEELCT product_name FROM products WHERE product_id = (?)"
  r <- quickQuery' conn query [toSql pid]
  disconnect conn
  return (map fromSql (head r))

getProdWithStock :: Connection -> Integer -> IO ProductWithStock
getProdWithStock db pid = do
  conn <- clone db
  let q = "select p.*, s.in_stock from products as p left join stock s on p.product_id = s.product_id left join products_sales_xref psx on p.product_id = psx.product_id where p.product_id=(?)"
  r <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ case r of
    [[]] -> E.throw NoDataError
    [x] -> desqlPS . tuplify4 $ x
    _ -> E.throw MultipleDataError

-- | getProdWithStocks returns all products, but tagged with their stock info.
getProdsWithStock :: Connection -> IO [ProductWithStock]
getProdsWithStock db = do
  conn <- clone db
  let q = "select p.*, s.in_stock from products as p left join stock s on p.product_id = s.product_id left join products_sales_xref psx on p.product_id = psx.product_id"
  r <- quickQuery' conn q []
  disconnect conn
  return $ case r of
    [[]] -> E.throw NoDataError
    x -> map (desqlPS . tuplify4) x

-- | Add a product into the database, returning True if it worked.
addProd :: Connection -> String -> Double -> IO Bool
addProd db name price = do
  when (name == "") (E.throw InvalidData)
  conn <- clone db
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
removeProd :: Connection -> Integer -> IO Bool
removeProd db pid = do
  conn <- clone db
  let q = "DELETE FROM products WHERE product_id=(?)"
  r <- run conn q [toSql pid]
  commit conn
  disconnect conn
  return $ r < 0

-- | Add a User into the database, returning True if it worked.
addUser :: Connection -> User -> IO Bool
addUser db usr = do
  conn <- clone db
  let q = "INSERT INTO users (user_name, user_password, user_privilege) VALUES (?, ?, ?)"
  pw <- quickHashPassword $ T.unpack usr.userPassword
  r <- run conn q [toSql usr.userName, toSql pw, toSql usr.userPrivilege]
  commit conn
  disconnect conn
  return $ r < 0

-- | Return a list of all of the users in the DB, excluding the passwords.
allCUsers :: Connection -> IO [CensoredUser]
allCUsers db = do
  conn <- clone db
  let q = "SELECT user_id, user_name, user_privilege FROM users"
  r <- quickQuery' conn q []
  disconnect conn
  return $ map (desqlCU . tuplify3) r

-- | Fetch a specific user, excluding the password.
getCUser :: Connection -> Integer -> IO CensoredUser
getCUser db uid = do
  conn <- clone db
  let q = "SELECT * FROM censored_users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlCU $ tuplify3 x
    _ -> E.throw MultipleDataError

-- | Get a specific user, including the password.
-- don't expose this through the API. duh.
getUser :: Connection -> Integer -> IO User
getUser db uid = do
  conn <- clone db
  let q = "SELECT * FROM users WHERE user_id=(?)"
  result <- quickQuery' conn q [toSql uid]
  disconnect conn
  when (result == [[]]) (E.throw NoDataError)
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlU $ tuplify4 x
    _ -> E.throw MultipleDataError

-- | Remove a user from the database.
removeUser :: Connection -> Integer -> IO Bool
removeUser db uid = do
  conn <- clone db
  let q = "DELETE FROM users WHERE user_id=(?)"
  r <- run conn q [toSql uid]
  commit conn
  disconnect conn
  return $ r < 0

-- | Test if there's anything in the users table, returning True if there is.
isUsersEmpty :: Connection -> IO Bool
isUsersEmpty db = do
  conn <- clone db
  let query = "SELECT COUNT(1) WHERE EXISTS (SELECT * FROM users)"
  result <- quickQuery' conn query []
  disconnect conn
  return $ case result of
    [[]] -> True
    [x] -> fromSql $ head x
    _ -> E.throw MultipleDataError

-- | Search the list of users by a string search query.
searchUsers :: Connection -> String -> IO [Integer]
searchUsers db term = do
  conn <- clone db
  let query = "SELECT user_id FROM users WHERE user_name LIKE (?)"
  let term' = "%" <> term <> "%"
  result <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of
    [[]] -> []
    x -> map (fromSql . head) x

getProd :: Connection -> Integer -> IO Product
getProd db pid = do
  conn <- clone db
  let q = "SELECT * FROM products WHERE product_id=(?)"
  result <- quickQuery' conn q [toSql pid]
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> E.throw MultipleDataError

-- | Returns a list of all the products in the database
allProds :: Connection -> IO [Product]
allProds db = do
  conn <- clone db
  let q = "SELECT * FROM products"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> []
    x -> map (desqlP . tuplify3) x

-- | Returns the most sold product in the database.
topProd :: Connection -> IO Product
topProd db = do
  conn <- clone db
  let q = "select product_id,product_name,product_price from best_product"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> head $ map (desqlP . tuplify3) result

-- | Returns the least sold product in the database.
bottomProd :: Connection -> IO Product
bottomProd db = do
  conn <- clone db
  let q = "select product_id,product_name,product_price from worst_product"
  result <- quickQuery' conn q []
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlP $ tuplify3 x
    _ -> head $ map (desqlP . tuplify3) result

-- | Searches the database for a product, returning a list of matching
-- | products.
searchProds :: Connection -> String -> IO [Product]
searchProds db term = do
  conn <- clone db
  let query = "SELECT * FROM products WHERE product_name LIKE (?)"
  let term' = "%" <> term <> "%"
  result <- quickQuery' conn query [toSql term']
  disconnect conn
  return $ case result of
    [[]] -> E.throw NoDataError -- handle this on caller side, throw a 404
    x -> map (desqlP . tuplify3) x

getSession :: Connection -> UUID -> IO Session
getSession db sid = do
  conn <- clone db
  let query = "SELECT * FROM sessions WHERE session_id = (?)"
  let query2 = "SELECT user_id FROM sessions WHERE session_id = (?)"
  result <- H.quickQuery' conn query [toSql . toString $ sid]
  result2 <- H.quickQuery' conn query2 [toSql . toString $ sid]
  let uid = (fromSql . head . head) result2
  disconnect conn
  cuser <- getCUser db uid
  return $ case result of
    [[]] -> E.throw NoDataError
    [x] -> desqlS (tuplify3 x) cuser
    _ -> E.throw MultipleDataError

addSession :: Connection -> Session -> IO Bool
addSession db sesh = do
  conn <- clone db
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
