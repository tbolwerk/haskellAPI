{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Main where
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Aeson       hiding (json)
import Data.IORef
import Data.Monoid      ((<>))
import Data.Text (Text,find)
import GHC.Generics
import Web.Spock
import Web.Spock.Config
import Data.HVect
import qualified Data.Configurator as C
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  firstName Text
  lastName Text
  zipCode Text
  address Text
  country Text
  city Text
  state Text
  houseNumber Int
  deriving Show
Product json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  description Text
  price Double
  quantity Int
  thumbnail Text IsNullable 
  images [ImageId]
  deriving Show
Image json
  url Text
  alt Text
|]

data APIState
   = APIState
   { as_cfg :: APIConfig
   }

data APIConfig
   = APIConfig
   { acfg_db   :: Text
   , acfg_port :: Int
   , acfg_name :: Text
   , bacfg_desc :: Text
   }

type API a = SpockM SqlBackend () APIState a
type APIAction a = SpockAction SqlBackend () APIState a

errorJson :: Int -> Text -> APIAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

usersAPI :: API ()
usersAPI = do
   get "users" $ do
    allPeople <- runSQL $ selectList [] [Asc UserId]
    json allPeople
   get ("users" <//> var) $ \userId -> do
    user <- runSQL $ selectFirst [UserId ==. userId] [Asc UserId]
    json user
   put ("users" <//> var) $ \userId -> do
    maybePerson <- jsonBody :: APIAction (Maybe User)
    case maybePerson of
       Nothing -> errorJson 1 "Failed to parse request body as User"
       Just thePerson -> do
          runSQL $ replace userId thePerson
   post "users" $ do
    maybePerson <- jsonBody :: APIAction (Maybe User)
    case maybePerson of
      Nothing -> errorJson 1 "Failed to parse request body as User"
      Just thePerson -> do
        newId <- runSQL $ insert thePerson
        json $ object ["result" .= String "success", "id" .= newId]

productsAPI :: API ()
productsAPI = do
   get "products" $ do
    allProducts <- runSQL $ selectList [] [Asc ProductId]
    json allProducts
   get ("products" <//> var) $ \productId -> do
    product <- runSQL $ selectFirst [ProductId ==. productId] [Asc ProductId]
    json product
   put ("products" <//> var) $ \productId -> do
    maybeProduct <- jsonBody :: APIAction (Maybe Product)
    case maybeProduct of
       Nothing -> errorJson 1 "Failed to parse request body as Product"
       Just theProduct -> do
          runSQL $ replace productId theProduct
   post "products" $ do
    maybeProduct <- jsonBody :: APIAction (Maybe Product)
    case maybeProduct of
      Nothing -> errorJson 1 "Failed to parse request body as Product"
      Just theProduct -> do
        newId <- runSQL $ insert theProduct
        json $ object ["result" .= String "success", "id" .= newId]

imagesAPI :: API ()
imagesAPI = 
    post "images" $ do
    maybeImage <- jsonBody :: APIAction (Maybe Image)
    case maybeImage of
      Nothing -> errorJson 1 "Failed to parse request body as Product"
      Just theImage -> do
        newId <- runSQL $ insert theImage
        json $ object ["result" .= String "success", "id" .= newId]

app :: API ()
app = do
  usersAPI
  productsAPI
  imagesAPI

parseConfig :: FilePath -> IO APIConfig
parseConfig cfgFile =
    do cfg <- C.load [C.Required cfgFile]
       db <- C.require cfg "db"
       port <- C.require cfg "port"
       name <- C.require cfg "apiName"
       desc <- C.require cfg "apiDescription"
       return (APIConfig db port name desc)

main :: IO ()
main = do
   cfg <- parseConfig "api.cfg"
   runApp cfg

runApp :: APIConfig -> IO ()
runApp acfg =
    do pool <- runNoLoggingT $ createSqlitePool (acfg_db acfg) 5
       cfg <- defaultSpockCfg () (PCPool pool) (APIState { as_cfg = acfg })
       runStdoutLoggingT $ runSqlPool (do runMigrationUnsafe migrateAll) pool
       runSpock (acfg_port acfg) (spock cfg app)
