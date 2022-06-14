{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

main :: IO ()
main = do
  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    middleware logStdoutDev
    home >> adm >> prod >> usr

data Product = Product
  { productId :: Int
  , productName :: String
  , productPrice :: Double
  } deriving Generic

data User = User
  { userId :: Int
  , userName :: String
  , hashedPassword :: String
  , privilege :: Int
  } deriving Generic

instance ToJSON Product
instance ToJSON User

home :: ScottyM ()
home = get "/" $ file "./public/index.html"

adm :: ScottyM ()
adm = get "/" $ file "./public/dash.html"

prod :: ScottyM ()
prod = get "/prods" $ json $ Product 1 "Strawberries" 1.99

usr :: ScottyM ()
usr = get "/users" $ json $ User 1 "Admin" "NULL" 1

