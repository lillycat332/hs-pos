{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Auth
-- License: BSD3
-- Stability: Stable
-- Portability: GHC
-- Description: Authentication functions for hs-pos.
-- This module contains functions for validating passwords, and for hashing
-- passwords for storage in the database.
module Database.HsPOS.Auth where

import Crypto.BCrypt qualified as P
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe (fromJust)
import Data.Text.Lazy qualified as T
import safe Database.HsPOS.Types (LoginRequest (requestPass))

-- | Validates a given login request, returning false if it's invalid.
validateCredentials :: LoginRequest -> BS.ByteString -> Bool
validateCredentials creds hashed = P.validatePassword hashed (pack (T.unpack creds.requestPass))

-- | Hash a password
quickHashPassword :: String -> IO String
quickHashPassword pass =
  P.hashPasswordUsingPolicy P.fastBcryptHashingPolicy (pack pass) >>= \bstring ->
    pure $ unpack (fromJust bstring)
