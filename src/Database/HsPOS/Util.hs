{- 
  hs-pos
  Util.hs
  Created by Lilly Cham on 16/09/2022

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

{-# LANGUAGE Trustworthy #-}
module Database.HsPOS.Util where
import Database.HDBC (fromSql, SqlValue)
import Database.HsPOS.Types
    ( CensoredUser(..), User(..), Product(Product) ) 
import qualified Data.Text.Lazy as T

tuplify3 :: [c] -> (c, c, c)
tuplify3 [x, y, z] = (x, y, z)

tuplify4 :: [d] -> (d, d, d, d)
tuplify4 [a, b, c, d] = (a,b,c,d)

{- | "DeSQL" a censored user (Int,Text,Int),
     ie. turn it from SQL types into a CensoredUser. -}
desqlCU :: (SqlValue, SqlValue, SqlValue) -> CensoredUser
desqlCU (id, name, priv)    =
  CensoredUser { cuser_id   = fromSql id
               , cuser_name = T.pack $ fromSql name
               , cuser_privilege = fromSql priv
               }

{- | "DeSQL" a user as a tuple (Int,Text,Int),
     ie. turn it from SQL types into a User . -}
desqlU :: (SqlValue, SqlValue, SqlValue, SqlValue) -> User
desqlU (id,name,pw,priv) =
  User { user_id        = fromSql id
       , user_name      = T.pack $ fromSql name
       , user_password  = T.pack $ fromSql pw
       , user_privilege = fromSql priv
       }

{- | "DeSQL" a product (Int,String,Double),
     ie. turn it from SQL types into Product. -}
desqlP :: (SqlValue, SqlValue, SqlValue) -> Product
desqlP (id, name, price) = Product (fromSql id)
                                   (fromSql name)
                                   (fromSql price)


