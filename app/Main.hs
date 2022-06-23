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
import qualified Data.Aeson as Aeson
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.Beam.Sqlite
import Database.Beam
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
  conn <- open "store.db"
  runBeamSqliteDebug putStrLn conn $ runInsert $
    insert (_users userDb) $
    insertValues [ User 0 "Admin" "NULL" 2]

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

-- usrs :: ScottyM ()
-- usrs = Web.Scotty.text $ show allUsers

data ProductT f 
  = Product
  { _productId     :: Columnar f Int32
  , _productName    :: Columnar f Text
  , _productPrice   :: Columnar f Double
  }
  	deriving (Generic, Beamable)

type Product = ProductT Identity
type ProductId = PrimaryKey UserT Identity

instance Table ProductT where
   data PrimaryKey ProductT f = ProductId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = ProductId . _productId

productDb :: DatabaseSettings be ProductDb
productDb = defaultDbSettings

data ProductDb f = ProductDb
              { _products :: f (TableEntity ProductT) }
                deriving (Generic, Database be)

data UserT f
  = User
  { _userId        :: Columnar f Int32
  , _userName      :: Columnar f Text
  , _userPassword  :: Columnar f Text
  , _userPrivilege :: Columnar f Int32 }
  deriving ( Generic, Beamable )

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = UserId . _userId

instance Aeson.ToJSON User where
  toJSON (User _userId _userName _userPassword _userPrivilege) = 
    object [ "id" .= _userId
           , "name" .= _userName
           , "passwd" .= _userPassword
           , "privilege" .= _userPrivilege
           ]
userDb :: DatabaseSettings be UserDb
userDb = defaultDbSettings

data UserDb f = UserDb
              { _users :: f (TableEntity UserT)  }
                deriving    (Generic, Database be)
