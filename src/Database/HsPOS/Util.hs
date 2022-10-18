{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Util
-- License: BSD3
-- Stability: Stable
-- Portability: GHC
-- Description: This module contains utility functions for hs-pos.
-- This module contains utility functions for hs-pos, primarily for converting
-- between types.
module Database.HsPOS.Util where

import Data.Maybe (fromJust)
import Data.Text.Lazy qualified as T
import Data.UUID (fromString)
import Database.HDBC (SqlValue, fromSql)
import Database.HsPOS.Session (Session (Session))
import Database.HsPOS.Types
  ( CensoredUser (..),
    Product (Product),
    ProductWithStock (ProductWithStock),
    User (..),
  )
import System.Random qualified as R

-- | Turns a list of 3 elements into a tuple of 3 elements
tuplify3 :: [c] -> (c, c, c)
tuplify3 [a, b, c] = (a, b, c)
tuplify3 _ = error "tuplify3: list must have 3 elements"

-- | Turns a list of 4 elements into a tuple of 4 elements
tuplify4 :: [d] -> (d, d, d, d)
tuplify4 [a, b, c, d] = (a, b, c, d)
tuplify4 _ = error "tuplify4: list must have 4 elements"

-- | "DeSQL" a censored user tuple (Int,Text,Int),
-- ie. turn it from SQL types into a CensoredUser.
desqlCU :: (SqlValue, SqlValue, SqlValue) -> CensoredUser
desqlCU (cuid, name, priv) =
  CensoredUser
    { cuserId = fromSql cuid,
      cuserName = T.pack $ fromSql name,
      cuserPrivilege = fromSql priv
    }

-- | "DeSQL" a user as a tuple (Int,Text,Int),
-- ie. turn it from SQL types into a User .
desqlU :: (SqlValue, SqlValue, SqlValue, SqlValue) -> User
desqlU (uid, name, pw, priv) =
  User
    { userId = fromSql uid,
      userName = T.pack $ fromSql name,
      userPassword = T.pack $ fromSql pw,
      userPrivilege = fromSql priv
    }

-- | "DeSQL" a product tuple (Int,String,Double),
-- ie. turn it from SQL types into Product.
desqlP :: (SqlValue, SqlValue, SqlValue) -> Product
desqlP (pid, name, price) =
  Product
    (fromSql pid)
    (fromSql name)
    (fromSql price)

-- | "DeSQL" a product tuple with a stock value(Int,String,Double, Int),
-- ie. turn it from SQL types into ProductWithStock
desqlPS :: (SqlValue, SqlValue, SqlValue, SqlValue) -> ProductWithStock
desqlPS (pid, name, price, inStock) =
  ProductWithStock
    ( Product
        (fromSql pid)
        (fromSql name)
        (fromSql price)
    )
    (fromSql inStock)

-- | Make a session from a session id, hash and a censored user.
desqlS :: (SqlValue, SqlValue, SqlValue) -> CensoredUser -> Session
desqlS (sid, _, hash) cuser =
  Session
    ((fromJust . fromString . fromSql) sid)
    cuser
    (fromSql hash)

-- | Creates a random generator with the specified seed value
getRandomGen :: Int -> R.StdGen
getRandomGen = R.mkStdGen

-- | Use the pre-seeded random generator to get a random seed
getRandomSeed :: IO Int
getRandomSeed = fst . R.random <$> R.getStdGen
