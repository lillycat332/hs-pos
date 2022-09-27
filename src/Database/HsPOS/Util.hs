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
import Database.HsPOS.Session ( Session(Session) )
import qualified Data.Text.Lazy as T
import qualified System.Random as R
import Data.UUID ( fromString )
import Data.Maybe (fromJust)
tuplify3 :: [c] -> (c, c, c)
tuplify3 [a, b, c] = (a, b, c)

tuplify4 :: [d] -> (d, d, d, d)
tuplify4 [a, b, c, d] = (a,b,c,d)

{- | "DeSQL" a censored user (Int,Text,Int),
     ie. turn it from SQL types into a CensoredUser. -}
desqlCU :: (SqlValue, SqlValue, SqlValue) -> CensoredUser
desqlCU (cuid, name, priv)    =
  CensoredUser { cuserId   = fromSql cuid
               , cuserName = T.pack $ fromSql name
               , cuserPrivilege = fromSql priv
               }

{- | "DeSQL" a user as a tuple (Int,Text,Int),
     ie. turn it from SQL types into a User . -}
desqlU :: (SqlValue, SqlValue, SqlValue, SqlValue) -> User
desqlU (uid,name,pw,priv) =
  User { userId        = fromSql uid
       , userName      = T.pack $ fromSql name
       , userPassword  = T.pack $ fromSql pw
       , userPrivilege = fromSql priv
       }

{- | "DeSQL" a product (Int,String,Double),
     ie. turn it from SQL types into Product. -}
desqlP :: (SqlValue, SqlValue, SqlValue) -> Product
desqlP (pid, name, price) = Product (fromSql pid)
                                    (fromSql name)
                                    (fromSql price)
desqlS :: (SqlValue, SqlValue, SqlValue) -> CensoredUser -> Session
desqlS (sid, _, hash) cuser = Session ((fromJust . fromString . fromSql) sid)
                                        cuser
                                       (fromSql hash)

-- | create a random generator with the specified seed value
getRandomGen :: Int -> R.StdGen
getRandomGen = R.mkStdGen


-- | Use the pre-seeded random generator to get a random seed
getRandomSeed :: IO Int
getRandomSeed = fst . R.random <$> R.getStdGen
