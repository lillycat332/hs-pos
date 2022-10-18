{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

-- | Module: Database.HsPOS.Session
-- License: BSD3
-- Stability: Stable
-- Portability: GHC
-- Description: Session type and the functions to generate and use them
-- This module contains the Session type and the functions to generate and use them.
module Database.HsPOS.Session where

import Data.Aeson qualified as A
import Data.Hashable qualified as H
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID) -- Using UUID V1, we don't need any features of newer UUID versions so they just add complexity.
import Database.HsPOS.Types (CensoredUser)
import GHC.Generics (Generic)

-- | Represents a session.
-- A session is a unique identifier that is used to identify a logged in user.
-- It consists of a UUID, a user, and a hash.
data Session where
  Session ::
    { sessionUUID :: UUID,
      sessionUser :: CensoredUser,
      sessionHash :: T.Text
    } ->
    Session
  deriving (Generic, Eq, Ord, Show, A.ToJSON, A.FromJSON)

-- | Generate a Session with a random UUID and a given user.
randomSession :: CensoredUser -> IO Session
randomSession u =
  nextUUID >>= \myUUID ->
    {- TLDR: making new session object from a random UUID and a user but also
    -- we have to convert to Text from String from Hash (Int) which makes it messy
    -- also pure is basically just "return" but it's more general since
    -- it's applicative and not just monadic
    -}
    pure $ Session {sessionUUID = fromJust myUUID, sessionUser = u, sessionHash = (T.pack . show . H.hash) u}
