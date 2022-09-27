{- 
  hs-pos
  Auth.hs
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


{-# LANGUAGE Trustworthy, ScopedTypeVariables, OverloadedRecordDot #-}

module Database.HsPOS.Auth where
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy           as T
import safe Database.HsPOS.Types ( LoginRequest, LoginRequest(requestPass) )
import qualified Crypto.BCrypt            as P
import Data.ByteString.Char8 ( pack, unpack )
import qualified Data.ByteString          as BS

-- | Validates a given login request, returning false if it's invalid.
validateCredentials :: LoginRequest -> BS.ByteString -> Bool
validateCredentials creds hashed = P.validatePassword hashed (pack (T.unpack creds.requestPass))

-- | Hash a password
quickHashPassword :: String -> IO String
quickHashPassword pass = P.hashPasswordUsingPolicy P.fastBcryptHashingPolicy (pack pass) >>= \bstring
  -> return $ unpack (fromJust bstring)
