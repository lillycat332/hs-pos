{- 
  hs-pos
  Types.hs
  Created by Lilly Cham on 7/5/22.

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
           , DuplicateRecordFields
           , GADTs
           , MultiParamTypeClasses
           , OverloadedRecordDot
           , OverloadedStrings 
           , ScopedTypeVariables 
           , TypeFamilies
           , TypeSynonymInstances 
           , Trustworthy
           , ImportQualifiedPost
#-}
{- HLINT ignore "Use camelCase" -}

module Database.HsPOS.Types where
import GHC.Generics ( Generic )
import Control.Exception qualified as E
import Data.Aeson qualified as A
import Data.Text.Lazy qualified as T
import Data.Hashable qualified as H
-- SQL Tables

data Product where
  Product :: { productId    :: Integer
             , productName  :: T.Text
             , productPrice :: Double
             } -> Product
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Product
instance A.FromJSON Product

data User where
  User :: { userId        :: Integer
          , userName      :: T.Text
          , userPassword  :: T.Text
          , userPrivilege :: Integer } -> User
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   User
instance A.FromJSON User

-- Same as User, but excluding the Password field (for sending to client)
-- Not actually a table, just an "illusion" of one, for the client.
-- Sort of like a view.
data CensoredUser where
  CensoredUser :: { cuserId        :: Integer
                  , cuserName      :: T.Text
                  , cuserPrivilege :: Integer
                  } -> CensoredUser
  deriving (Eq, Ord, Show, Generic, H.Hashable)

instance A.ToJSON   CensoredUser
instance A.FromJSON CensoredUser

data Stock where
  Stock :: { stockId   :: Integer
           , inStock   :: T.Text
           } -> Stock
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Stock
instance A.FromJSON Stock

data Sale where
  Sale :: { salesId    :: Integer
          , salesDate  :: Date
          , numberSold :: Integer
          } -> Sale
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Sale
instance A.FromJSON Sale

data ProductsSalesXref where
  ProductsSalesXref :: { xref_product_id :: Integer  -- Foreign key to product_id
                       , xref_sales_id   :: Integer  -- Foreign key to sales_id
                       } -> ProductsSalesXref
  deriving (Eq, Ord, Show, Generic)

data Till where
  Till :: { tillId   :: Integer
          , tillName :: T.Text
          } -> Till
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Till
instance A.FromJSON Till

data UserTillXref where
  UserTillXref :: { xrefUserId   :: Integer  -- Foreign key to user_id
                  , xrefTillId   :: Integer  -- Foreign key to till_id
                  } -> UserTillXref
  deriving (Eq, Ord, Show, Generic)

data Date where
  Date :: { year    :: Integer
          , month   :: Integer
          , dateDay :: Day
          } -> Date
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Date
instance A.FromJSON Date

data Day where
  Day :: { day    :: Integer
         , hour   :: Integer
         , minute :: Integer
         , second :: Integer
         } -> Day
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Day
instance A.FromJSON Day

data LoginRequest where
  LoginRequest :: { requestName :: T.Text
                  , requestPass :: T.Text
                  } -> LoginRequest
                  
  deriving ( Eq, Ord, Show
           , Read, Generic
           , A.FromJSON , A.ToJSON)

data APIError = InvalidData
  deriving (Show, E.Exception)

data DBError = NoDataError
  deriving (Show, E.Exception)

{- Smart Constructors
   These are constructors for our datatypes. They're "smart" because
   they impose restrictions on the construction of said type, making
   illegal state unrepresentable -}
-- 
isDateValid :: (Integral a1, Ord a2, Num a2, Num a3, Eq a3) => a1 -> a3 -> a2 -> Bool
isDateValid y m d
  | y < 1970 || y > 9999 || d < 1  = False
  | m `elem` [1,3,5,7,8,10,12]     = d <= 31
  | m `elem` [4,6,9,11]            = d <= 30
  | m == 2                         = d <= if isLeapYear then 29 else 28
  | otherwise                      = False
  where isLeapYear = y `mod` 400 == 0 || (y `mod` 100 /= 0 && y `mod` 4 == 0)
  
-- | mkDate is the smart constructor for dates. It will throw an 
-- assertion error if a date is invalid.
mkDate :: Integer -> Integer -> Day -> Date
mkDate y m d  = E.assert (isDateValid y m (d.day)) $ Date y m d

-- | mkDay is the smart constructor for days. It will throw an 
-- assertion error if a day is invalid.
mkDay :: Integer -> Integer -> Integer -> Integer -> Day
mkDay d h m s = E.assert (isDayValid d h m s) $ Day d h m s

isDayValid :: (Ord a1, Ord a2, Ord a3, Ord a4, Num a1, Num a2, Num a3, Num a4) => a1 -> a2 -> a3 -> a4 -> Bool
isDayValid d h m s 
  | d >   1 || d <= 31 = False
  | h >=  0 || h <  23 = False
  | m >=  0 || m <  59 = False
  | s >=  0 || s <  59 = False
  | otherwise          = False

-- Converters
censorUser :: User -> CensoredUser
censorUser u = CensoredUser u.userId u.userName u.userPrivilege
