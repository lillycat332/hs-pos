{-# LANGUAGE ImportQualifiedPost
           , GADTs
           , DeriveGeneric
           , DeriveAnyClass
           , Trustworthy
#-}

module Database.HsPOS.Session where
import Database.HsPOS.Types ( CensoredUser )
import Data.UUID.V1 ( nextUUID )
import Data.Maybe ( fromJust )
import Data.UUID ( UUID )
import GHC.Generics (Generic)
import Data.Hashable qualified as H
import Data.Text qualified as T
import Data.Aeson qualified as A

data Session where
  Session :: { sessionUUID  :: UUID
             , sessionUser  :: CensoredUser
             , sessionHash  :: T.Text
             } -> Session
  deriving (Generic, Eq, Ord, Show, A.ToJSON, A.FromJSON)

randomSession :: CensoredUser -> IO Session
randomSession u = nextUUID >>= \myUUID ->
  return $ Session { sessionUUID = fromJust myUUID, sessionUser = u, sessionHash = (T.pack . show . H.hash) u}

