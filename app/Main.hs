{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , MultiParamTypeClasses
           , ScopedTypeVariables 
           , StandaloneDeriving 
           , TypeFamilies
           , TypeSynonymInstances 
           , OverloadedStrings
           , TypeApplications
           , OverloadedLabels
#-}
module Main where      
import Control.Applicative
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson ((.=))
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Options.Applicative hiding (header)
import qualified Data.Aeson as Aeson
import qualified Options.Applicative as Opt
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
import Database.HDBC
import qualified Database.HDBC as H
import Database.HDBC.Sqlite3 (connectSqlite3)


data Args = Args
  { optDb     :: String
  , optPort   :: Int
  , optQuiet  :: Bool
  }

args :: Parser Args
args = Args <$> 
  strOption
  (  long "db"
  <> short 'd'
  <> help "Path to the database store"
  <> showDefault
  <> value "store.db"
  <> metavar "<path to file>"
  ) <*> 
  option auto
  (  help "The port number to serve on"
  <> long "port"
  <> metavar "<1-65535>"
  <> short 'p'
  <> showDefault
  <> value 3000
  ) <*>
  switch
  (  long "quiet"
  <> short 'q'
  <> help "Don't print debug info to stdout"
  )

main :: IO ()
main = do
  opts <- execParser opts
  let port = optPort opts

  -- withSQLite (optDb opts) $ do
  --   tryCreateTable users
  --   tryCreateTable products
  --   usersSelect <- query $ do
  --     user <- select users
  --     return (user ! #user_name :*: user ! #user_privilege)
  --   liftIO $ print usersSelect

  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public") 
    if optQuiet opts /= True
      then middleware logStdoutDev
      else return ()
    home >> adm >> restHandle
  
  where
    opts = info (args <**> helper)
      (  fullDesc
      <> progDesc "Run a server for an EPOS system, with a REST API and Database."
      <> Opt.header "hs-pos -- A Haskell EPOS backend"
      )


-- Connection handlers

home :: ScottyM ()
home = get "/" $ file "./public/index.html"

adm :: ScottyM ()
adm = get "/dashboard" $ file "./public/dash.html"

restHandle :: ScottyM ()
restHandle = do 
  get "/users/:u" $ do
    html "get"
  post "/users/:u" $ do
    html "post"
  delete "/users/:u" $ do 
    html "delete"
  put "/users/:u" $ do
    html "put"
  
  get "/prods/:u" $ do
    html "get"
  post "/prods/:u" $ do
    html "post"
  delete "/prods/:u" $ do 
    html "delete"
  put "/prods/:u" $ do
    html "put"

-- saleHandle :: ScottyM () 
-- saleHandle = do
--   get "/sales/:date/:id/" $ do 
--     id    <- param "id"
--     date  <- param "date"
--     sales <- monthlySales "store.db" date id
--     html . fromSql sales

-- SQL Tables

-- data Product = Product
--   { product_id   :: Int
--   , product_name :: Text
--   , product_price :: Double
--   } deriving Generic
-- instance SqlRow Product

-- data User = User
--   { user_id        :: Int
--   , user_name      :: Text
--   , user_password  :: Text
--   , user_privilege :: Int
--   } deriving Generic
-- instance SqlRow User

-- users :: Table User
-- users = table "users" [#user_id :- primary]

-- products :: Table Product
-- products = table "products" [#product_id :- primary]

-- SQL functions

-- | monthlySales takes:
-- c, a connection,
-- d, a date in the format "YYYY-MM",
-- id, a product id, 
-- and returns the total sales for that month.
monthlySales :: String -> String -> Int -> IO [[SqlValue]]
monthlySales c d id = do
  conn <- connectSqlite3 c
  let q = "SELECT SUM(number_sold) FROM sales INNER JOIN products_sales_xref ON products_sales_xref.sales_id = sales.sales_id WHERE strftime('%Y-%m', sales_date) = (?) AND products_sales_xref.product_id = (?)"
  r <- H.quickQuery' conn q [toSql d, toSql id]
  disconnect conn
  return r

-- instance Aeson.ToJSON User where
--   toJSON (User _userId _userName _userPassword _userPrivilege) = 
--     Aeson.object [ "id" .= _userId
--                  , "name" .= _userName
--                  , "passwd" .= _userPassword
--                  , "privilege" .= _userPrivilege
--                  ] 