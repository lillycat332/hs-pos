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
           , MultiParamTypeClasses
           , OverloadedRecordDot
           , OverloadedStrings 
           , ScopedTypeVariables 
           , TypeFamilies
           , TypeSynonymInstances 
           , Trustworthy
#-}
{-# LANGUAGE GADTs #-}
{- HLINT ignore "Use camelCase" -}

module Database.HsPOS.Types where
import Data.Aeson ((.=))
import GHC.Generics ( Generic )
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
-- SQL Tables

data Product = Product
  { product_id    :: Integer
  , product_name  :: T.Text
  , product_price :: Double
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Product
instance A.FromJSON Product

data User where
  User :: {user_id :: Integer,
             user_name :: T.Text,
             user_password :: T.Text,
             user_privilege :: Integer}
            -> User
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   User
instance A.FromJSON User

-- Same as User, but excluding the Password field (for sending to client)
-- Not actually a table, just an "illusion" of one, for the client.
-- Sort of like a view.
data CensoredUser = CensoredUser
  { cuser_id        :: Integer
  , cuser_name      :: T.Text
  , cuser_privilege :: Integer
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   CensoredUser
instance A.FromJSON CensoredUser

data Stock = Stock
  { stock_id   :: Integer
  , in_stock   :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Stock
instance A.FromJSON Stock

data Sale = Sale
  { sales_id    :: Integer
  , sales_date  :: Date
  , number_sold :: Integer
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Sale
instance A.FromJSON Sale

data Products_sales_xref = Products_sales_xref
  { xref_product_id :: Integer  -- Foreign key to product_id
  , xref_sales_id   :: Integer  -- Foreign key to sales_id
  } deriving (Eq, Ord, Show, Generic)

data Till = Till
  { till_id   :: Integer
  , till_name :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Till
instance A.FromJSON Till

data User_till_xref = User_till_xref
  { xref_user_id   :: Integer  -- Foreign key to user_id
  , xref_till_id   :: Integer  -- Foreign key to till_id
  } deriving (Eq, Ord, Show, Generic)

data Date = Date
  { year    :: Integer
  , month   :: Integer
  , dateDay :: Day
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Date
instance A.FromJSON Date

data Day = Day { day    :: Integer
               , hour   :: Integer
               , minute :: Integer
               , second :: Integer
               } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Day
instance A.FromJSON Day

data Request = Request { requestName :: T.Text
                       , requestPass :: T.Text
                       } deriving ( Eq, Ord, Show
                                  , Read, Generic
                                  , A.FromJSON , A.ToJSON)
{- Smart Constructors
   These are constructors for our datatypes. They're "smart" because
   they impose restrictions on the construction of said type, making
   illegal state unrepresentable! -}

isDateValid y m d
  | y < 1970 || y > 9999 || d < 1  = False
  | m `elem` [1,3,5,7,8,10,12]     = d <= 31
  | m `elem` [4,6,9,11]            = d <= 30
  | m == 2                         = d <= if isLeapYear then 29 else 28
  | otherwise                      = False
  where isLeapYear = y `mod` 400 == 0 || (y `mod` 100 /= 0 && y `mod` 4 == 0) 

mkDate y m d  = E.assert (isDateValid y m (d.day)) $ Date y m d

mkDay d h m s = E.assert (isDayValid d h m s) $ Day d h m s

isDayValid d h m s 
  | d >   1 || d <= 31 = False
  | h >=  0 || h <  23 = False
  | m >=  0 || m <  59 = False
  | s >=  0 || s <  59 = False
  | otherwise          = False
