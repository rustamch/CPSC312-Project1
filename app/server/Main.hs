{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Lib
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Network.Wai.Middleware.RequestLogger

import Control.Concurrent.STM
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Reader

import Web.Scotty.Trans

main :: IO ()
main = do
    sync <- newTVarIO (State [])
    let runActionToIO m = runReaderT (runWebM m) sync
    scottyT 3000 runActionToIO app

data UserCreateRequest = UserCreateRequest {
  name :: String,
  id :: String
} deriving (Show, Generic)

getName :: UserCreateRequest -> String
getName (UserCreateRequest name _) = name

instance ToJSON UserCreateRequest
instance FromJSON UserCreateRequest

newtype WebM a = WebM { runWebM :: ReaderT (TVar State) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar State), MonadUnliftIO)

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

modify f = ask >>= liftIO . atomically . flip modifyTVar' f

app :: ScottyT WebM ()
app = do
    middleware logStdoutDev
    get "/ping" $ do
      text "pong"
    post "/user" $ do
      (UserCreateRequest name id) <- jsonData :: ActionT WebM UserCreateRequest
      webM $ modify $ \ st -> createUser st name id
      json $ UserCreateRequest name id
    post "/manyusers" $ do
      users <- jsonData :: ActionT WebM [UserCreateRequest]
      webM $ modify $ \ st -> foldl (\st (UserCreateRequest name id) -> createUser st name id) st users
      json $ users
    get "/allusers" $ do
      users <- webM $ getUsers <$> (ask >>= liftIO . readTVarIO)
      json users
    get "/user/:id" $ do
      id <- param "id"
      state <- webM $ ask >>= liftIO . readTVarIO
      let user = getUserById state id
      json user
    post "/message" $ do
      message <- jsonData :: ActionT WebM Message
      webM $ modify $ \ st -> sendMsg st message
      state <- webM $ ask >>= liftIO . readTVarIO
      json $ state
    get "/chats/:id" $ do
      id <- param "id"
      state <- webM $ ask >>= liftIO . readTVarIO
      let chats = getChats state id
      json chats