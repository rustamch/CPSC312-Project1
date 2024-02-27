{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (unless)
import Network.HTTP.Simple
import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import System.Console.ANSI

import Lib

exit :: IO ()
exit = do
    putStrLn "Goodbye!"
    return ()

main :: IO ()
main = do
    clearScreen
    putStrLn "===============WELCOME TO THE CHAT CLIENT!==============="
    putStrLn "Enter your user id to login (or a new username to create a new user):"
    id <- getLine
    request <- parseRequest ("http://localhost:3000/user/"++id)
    response <- httpLBS request
    let userM = decode $ getResponseBody response
    case userM of
        Just user -> do
            selectChat $ user
            loop 
        Nothing -> do
            putStrLn "User not found, create new user? [y/N]"
            confirm <- getLine
            if confirm `elem` ["y", "Y", "yes"] then do
                createNewUser
            else exit

createNewUser :: IO ()
createNewUser = do
    putStrLn "Enter a username:"
    name <- getLine
    putStrLn "Enter a user id:"
    id <- getLine
    putStrLn "Are you sure you want to create a new user with the following details?"
    putStrLn $ "Name: " ++ name
    putStrLn $ "ID: " ++ id
    putStrLn "y/N"
    confirm <- getLine
    if confirm `elem` ["y", "Y", "yes"] then do
        request <- parseRequest "POST http://localhost:3000/user"
        let user = User name id []
        let req = setRequestBodyJSON user request
        response <- httpLBS req
        putStrLn $ "Created user: " ++ (BL.unpack $ getResponseBody response)
        selectChat user
        loop
    else loop

printAllChatOpponents :: User -> IO ()
printAllChatOpponents (User name _ chats) = do
    putStrLn "Select a chat or start a new chat by entering your friend's username:"
    forM_ chats $ \(Chat p1 p2 _) -> do
        putStrLn $ "|- " ++ (if p1 == name then p2 else p1)
    putStrLn "==========================="

printAllMessages :: String -> Chat -> IO ()
printAllMessages username (Chat _ _ messages) = do
    forM_ messages $ \(Message fro to msg) -> do
        putStrLn $ (if fro == username then "You" else fro) ++ ": " ++ msg

selectChat :: User -> IO ()
selectChat user = do
    clearScreen
    putStrLn ("Welcome, "++(name user)++"!")
    printAllChatOpponents user
    targetChat <- getLine
    checkIfBackToMenu targetChat (selectChatIO user targetChat) user

selectChatIO :: User -> String -> IO ()
selectChatIO user targetChat = do
    putStrLn ("Selecting chat..."++targetChat++"..."++(username user))
    let chat = getChat (chats user) (username user) targetChat
    case chat of
        Just c -> do
            updateAndDisplayChat user targetChat
            loop
        Nothing -> do
            putStrLn "Chat not found, create new chat?"
            createNewChat user targetChat

updateAndDisplayChat :: User -> String -> IO ()
updateAndDisplayChat (User name id _) p2 = do
    clearScreen
    request <- parseRequest ("http://localhost:3000/username/"++name)
    response <- httpLBS request
    let user = decode $ getResponseBody response
    case user of
        Just u -> do
            let chatM = getChat (chats u) (username u) p2
            case chatM of
                Just chat -> do
                    printAllMessages name chat
                Nothing -> do
                    putStrLn "Chat not found, create new chat?"
                    createNewChat u p2
        Nothing -> do
            putStrLn "Impossible"
            error "Impossible"

checkUserExists :: String -> IO Bool
checkUserExists name = do
    request <- parseRequest ("http://localhost:3000/username/"++name)
    response <- httpLBS request
    putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
    let userM :: Maybe User = decode $ getResponseBody response
    putStrLn $ "User: " ++ (show userM)
    case userM of
        Nothing -> 
            return False
        _ -> return True

createNewChat :: User -> String -> IO ()
createNewChat user targetUser = do
    putStrLn "Are you sure you want to create a new chat with the following user?"
    putStrLn $ "Username: " ++ targetUser
    putStrLn "y/N"
    confirm <- getLine
    if confirm `elem` ["y", "Y", "yes"] then do
        exists <- checkUserExists targetUser
        case exists of
            True -> do
                sendMessageIO user targetUser
                updateAndDisplayChat user targetUser
                loop
            False -> do
                putStrLn "User not found, trying again..."
                promptAndCreateNewChat user
    else selectChat user

promptAndCreateNewChat :: User -> IO ()
promptAndCreateNewChat user = do
    putStrLn "Enter the username of the user you want to chat with: [enter 'back' to go back to the menu]"
    targetUser <- getLine
    checkIfBackToMenu targetUser (createNewChat user targetUser) user

sendMessageIO :: User -> String -> IO ()
sendMessageIO user targetUser = do
    putStrLn "Enter your message: [enter 'back' to go back to the menu]"
    putStrLn "==========================="
    msg <- getLine
    checkIfBackToMenu msg (sendMessageServer msg user targetUser) user

sendMessageServer :: String -> User -> String -> IO ()
sendMessageServer msg user target = do
    request <- parseRequest "POST http://localhost:3000/message"
    let message = Message (username user) target msg
    let req = setRequestBodyJSON message request
    response <- httpLBS req
    -- putStrLn $ "Sent message: " ++ (BL.unpack $ getResponseBody response)
    updateAndDisplayChat user target

checkIfBackToMenu :: String -> IO () -> User -> IO ()
checkIfBackToMenu input action user = do
    if input == "back" then selectChat user else action

loop :: IO ()
loop = do
    line <- getLine
    unless (line == "quit") $ do
        --request <- parseRequest "http://localhost:3000/ping"
        --response <- httpLBS request
        --putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
        putStrLn $ "You typed: " ++ line
        loop