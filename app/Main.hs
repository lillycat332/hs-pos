{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , MultiParamTypeClasses
           , ScopedTypeVariables 
           , StandaloneDeriving 
           , TypeFamilies
           , TypeSynonymInstances 
           , OverloadedStrings
#-}
module Main where
import Control.Applicative
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

main :: IO ()
main = do
  -- conn <- open "store.db"
  -- runBeamSqliteDebug putStrLn conn $ runInsert $
  --   insert (_users userDb) $
  --   insertValues [ User "Admin" "NULL" 2]

  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    middleware logStdoutDev
    home >> adm

-- Connection handler

home :: ScottyM ()
home = get "/" $ file "./public/index.html"

adm :: ScottyM ()
adm = get "/" $ file "./public/dash.html"