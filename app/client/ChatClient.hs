module Main where

import Control.Monad (unless)
import Network.HTTP.Simple
import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson

import Lib

main :: IO ()
main = do
    putStrLn "User id:"
    id <- getLine
    request <- parseRequest ("http://localhost:3000/user/"++id)
    response <- httpLBS request
    putStrLn $ "Logged in with user: " ++ (BL.unpack $ getResponseBody response)
    let userM = decode $ getResponseBody response
    case userM of
        Just user -> do
            putStrLn "Welcome to the chat client!"
            selectChat $ user
            loop 
        Nothing -> putStrLn "User not found, create new user?"
    

printAllChatOppoents :: User -> IO ()
printAllChatOppoents (User name _ chats) = do
    putStrLn "Select a chat:"
    forM_ chats $ \(Chat p1 p2 _) -> do
        putStrLn $ "- " ++ (if p1 == name then p2 else p1)
    putStrLn "==========================="

printAllMessages :: User -> Chat -> IO ()
printAllMessages (User _ id _) (Chat _ _ messages) = do
    forM_ messages $ \(Message fro to msg) -> do
        putStrLn $ (if fro == id then "You" else fro) ++ ": " ++ msg

selectChat :: User -> IO ()
selectChat user = do
    printAllChatOppoents user
    targetChat <- getLine
    let chat = getChat (chats user) (userId user) targetChat
    printAllMessages user chat

loop :: IO ()
loop = do
    line <- getLine
    unless (line == "quit") $ do
        --request <- parseRequest "http://localhost:3000/ping"
        --response <- httpLBS request
        --putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
        putStrLn $ "You typed: " ++ line
        loop