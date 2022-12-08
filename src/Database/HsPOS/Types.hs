{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Types
-- License: BSD3
-- Stability: Stable
-- Portability: GHC
-- Description: Types used in the hs-pos library, and operations on them.
-- This module contains types used in the hs-pos library, and operations on them.
module Database.HsPOS.Types where

import Control.Exception qualified as E
import Data.Aeson qualified as A
import Data.Hashable qualified as H
import Data.Text.Lazy qualified as T
import Data.Time.Calendar.OrdinalDate qualified as C
import GHC.Generics (Generic)

-- | A product.
data Product where
  Product ::
    { productId :: Integer,
      productName :: T.Text,
      productPrice :: Double
    } ->
    Product
  deriving (Eq, Ord, Show, Generic, A.ToJSON, A.FromJSON)

-- | A union with a product and a quantity of that product that is currently
-- in stock.
data ProductWithStock where
  ProductWithStock ::
    { prod :: Product,
      pInStock :: Integer
    } ->
    ProductWithStock
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON ProductWithStock

instance A.FromJSON ProductWithStock

-- | A user.
data User where
  User ::
    { userId :: Integer,
      userName :: T.Text,
      userPassword :: T.Text,
      userPrivilege :: Integer
    } ->
    User
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON User

instance A.FromJSON User

-- | Same as User, but excluding the Password field (for sending to client)
-- Not actually a table, just an "illusion" of one, for the client.
-- Sort of like a view. In the database, there is a view table for this.
data CensoredUser where
  CensoredUser ::
    { cuserId :: Integer,
      cuserName :: T.Text,
      cuserPrivilege :: Integer
    } ->
    CensoredUser
  deriving (Eq, Ord, Show, Generic, H.Hashable)

instance A.ToJSON CensoredUser

instance A.FromJSON CensoredUser

-- | A representation of the stock table.
data Stock where
  Stock ::
    { stockId :: Integer,
      inStock :: T.Text
    } ->
    Stock
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON Stock

instance A.FromJSON Stock

-- | A representation of the sales table.
data Sale where
  Sale ::
    { salesId :: Integer,
      salesDate :: C.Day,
      numberSold :: Integer
    } ->
    Sale
  deriving (Eq, Ord, Show, Generic, A.FromJSON, A.ToJSON)

-- | Product with sales data attached.
data ProductSale where
  ProductSale ::
    { saleProduct :: Product,
      saleDate :: C.Day,
      saleQuantity :: Integer
    } ->
    ProductSale
  deriving (Eq, Ord, Show, Generic, A.ToJSON, A.FromJSON)

-- | A representation of the Products <--> Sales cross-reference table.
-- Not really used, but it's here for completeness.
data ProductsSalesXref where
  ProductsSalesXref ::
    { xref_product_id :: Integer, -- Foreign key to product_id
      xref_sales_id :: Integer -- Foreign key to sales_id
    } ->
    ProductsSalesXref
  deriving (Eq, Ord, Show, Generic)

-- | A login request.
data LoginRequest where
  LoginRequest ::
    { requestName :: T.Text,
      requestPass :: T.Text
    } ->
    LoginRequest
  deriving
    ( Eq,
      Ord,
      Show,
      Read,
      Generic,
      A.FromJSON,
      A.ToJSON
    )

-- | A login response.
-- This is mostly just a workaround of some weird JSON bug with top level values
data IsOk where
  IsOk :: {ok :: Bool} -> IsOk
  deriving (Generic, Eq, Show, A.ToJSON)

data PredSale where
  PredSale :: {predSale :: Double} -> PredSale
  deriving (Generic, Eq, Show, A.ToJSON)

-- Error types

-- | Indicates failure of an API call.
data APIError = InvalidData
  deriving (Show, E.Exception)

-- | Indicates failure of a database call.
-- This is a bit of a misnomer, as it's not really a database error, but rather
-- a failure to parse the database response.
-- The NoDataError is thrown when the database returns no rows.
-- The MultipleDataError is thrown when the database returns more than one row
-- when it should only return one.
data DBError = NoDataError | MultipleDataError
  deriving (Show, E.Exception)

-- Converters

-- | Censors a user, removing the password field.
censorUser :: User -> CensoredUser
censorUser u = CensoredUser u.userId u.userName u.userPrivilege
