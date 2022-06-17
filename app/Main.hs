{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , ScopedTypeVariables 
           , StandaloneDeriving 
           , TypeSynonymInstances 
           , MultiParamTypeClasses
#-}
module Main where
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text
import Database.Beam
import Database.Beam.Sqlite
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
import Control.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = do
  -- conn <- connectSqlite3 "store.db"
  -- run conn "CREATE TABLE IF NOT EXISTS users(id INTEGER AUTO_INCREMENT, Username CHAR, HashedPassword CHAR, Privilege INTEGER);" []
  -- disconnect conn
  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    middleware logStdoutDev
    home >> adm

-- data Product = Product
--   { productId :: Int
--   , productName :: String
--   , productPrice :: Double
--   } deriving Generic

data UserT f
  = User
  { _userName      :: Columnar f Text
  , _userPassword  :: Columnar f Text
  , _userPrivilege :: Columnar f Int32
  }
  deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity
instance Beamable UserT
instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
   primaryKey = UserId . _userEmail

data userDb f = userDb
              { _users :: f (TableEntity UserT) }
                deriving (Generic, Database be)

userDb :: DatabaseSettings be userDb
userDb = defaultDbSettings

-- Connection handler

home :: ScottyM ()
home = get "/" $ file "./public/index.html"

adm :: ScottyM ()
adm = get "/" $ file "./public/dash.html"

-- prod :: ScottyM ()
-- prod = get "/prods" $ json "hi"

-- usr :: ScottyM ()
-- usr = get "/users" $ json "bye"

-- Database handling

addUser :: String -> String -> Integer -> IO [[SqlValue]]
addUser name pw priv = 
  do
    conn <- connectSqlite3 "store.db"
    state <- prepare conn "INSERT INTO users VALUES (?,?,?)" 
    execute state [toSql name, toSql pw, toSql priv]
    result <- fetchAllRows' state
    putStrLn $ show result

    disconnect conn

    return result

