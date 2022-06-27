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
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.HDBC
import Database.HDBC.Sqlite3
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Options.Applicative as Opt
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
          
db = strOption
       (  long "db"
       <> short 'd'
       <> help "Path to the database store"
       <> showDefault
       <> value "store.db"
       <> metavar "<path to file>"
       )

port = option auto
       (  long "port"
       <> short 'p'
       <> help "The port number to serve on"
       <> showDefault
       <> value 3000
       <> metavar "<1-65535>"
       )

data Args = Args
  { optDb     :: String
  , optPort   :: Int
  }

args :: Parser Args
args = Args <$> db <*> port

main :: IO ()
main = do
  opts <- execParser opts
  let port = optPort opts

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    middleware logStdoutDev
    home >> adm
  
  where
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Run a server for an EPOS system, with a REST API and Database."
      <> Opt.header "hs-pos -- A Haskell EPOS backend" )


-- Connection handler

home :: ScottyM ()
home = get "/" $ file "./public/index.html"

adm :: ScottyM ()
adm = get "/" $ file "./public/dash.html"

data Product
  = Product
  { _productId      :: Integer
  , _productName    :: Text
  , _productPrice   :: Double
  } deriving         ( Generic )

data UserT f
  = User
  { userId        :: Integer
  , userName      :: Text
  , userPassword  :: Text
  , userPrivilege :: Integer 
  } deriving       ( Generic )

-- instance Aeson.ToJSON User where
--   toJSON (User _userId _userName _userPassword _userPrivilege) = 
--     Aeson.object [ "id" .= _userId
--                  , "name" .= _userName
--                  , "passwd" .= _userPassword
--                  , "privilege" .= _userPrivilege
--                  ]