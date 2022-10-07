{-
  hs-pos
  Session.hs
  Created by Lilly Cham

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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Trustworthy #-}

-- | This module contains a session type and the functions to generate and use it.
module Database.HsPOS.Session where

import Data.Aeson qualified as A
import Data.Hashable qualified as H
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
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
