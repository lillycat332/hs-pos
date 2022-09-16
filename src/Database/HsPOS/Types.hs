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


{-# LANGUAGE Trustworthy
           , DeriveAnyClass
           , DeriveGeneric
           , MultiParamTypeClasses
           , ScopedTypeVariables 
           , StandaloneDeriving 
           , TypeFamilies
           , TypeSynonymInstances 
           , OverloadedStrings
           , TypeApplications
           , OverloadedLabels
           , OverloadedRecordDot
#-}


module Database.HsPOS.Types where      
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as T
import Data.Aeson ((.=))
import GHC.Generics
-- SQL Tables

data Product = Product
  { product_id    :: Int
  , product_name  :: T.Text
  , product_price :: Double
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Product
instance A.FromJSON Product


data User = User
  { user_id        :: Int
  , user_name      :: T.Text
  , user_password  :: T.Text
  , user_privilege :: Int
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   User
instance A.FromJSON User

-- Same as User, but excluding the Password field (for sending to client)
-- Not actually a table, just an "illusion" of one, for the client.
-- Sort of like a view.
data CensoredUser = CensoredUser
  { cuser_id        :: Int
  , cuser_name      :: T.Text
  , cuser_privilege :: Int
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   CensoredUser
instance A.FromJSON CensoredUser

data Stock = Stock
  { stock_id   :: Int
  , in_stock   :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Stock
instance A.FromJSON Stock

data Sale = Sale
  { sales_id    :: Int
  , sales_date  :: String
  , number_sold :: Int
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Sale
instance A.FromJSON Sale

data Products_sales_xref = Products_sales_xref
  { xref_product_id :: Int  -- Foreign key to product_id
  , xref_sales_id   :: Int  -- Foreign key to sales_id
  } deriving (Eq, Ord, Show, Generic)

data Till = Till
  { till_id   :: Int
  , till_name :: T.Text
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Till
instance A.FromJSON Till

data User_till_xref = User_till_xref
  { xref_user_id   :: Int  -- Foreign key to user_id
  , xref_till_id   :: Int  -- Foreign key to till_id
  } deriving (Eq, Ord, Show, Generic)

data Date = Date
  { dateYear  :: Int
  , dateMonth :: Int
  , dateDay   :: Int
  } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON   Date
instance A.FromJSON Date

data Day = Day { dayHour   :: Int
               , dayMinute :: Int
               , daySecond :: Int
               } deriving (Eq, Ord, Show, Generic)
instance A.ToJSON   Day
instance A.FromJSON Day

data Request = Request { requestName :: String
                       , requestPass :: String
                       } deriving ( Eq, Ord, Show
                                  , Read, Generic
                                  , A.FromJSON , A.ToJSON)

{- Marshalling To JSON
   These convert SQL table objects as above into JSON objects and vice
   versa.
-}
