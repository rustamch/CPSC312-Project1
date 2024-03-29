{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (unless)
import Network.HTTP.Simple
import Control.Monad
import Control.Concurrent
import Debug.Trace
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
    putStrLn "===========================WELCOME TO THE CHAT CLIENT!================================"
    putStrLn "Enter '\\exit' to exit the program at any time, and'\\back' to come back to this page."
    putStrLn "--------------------------------------------------------------------------------------"
    putStrLn "Enter your user id to login (or a new id to create a new user):"
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
        e1 <- checkUserExists name
        e2 <- checkUserExists id
        if e1 || e2 then do
            clearScreen
            putStrLn "User already exists, try again..."
            createNewUser
        else do
            request <- parseRequest "POST http://localhost:3000/user"
            let user = User name id []
            let req = setRequestBodyJSON user request
            response <- httpLBS req
            -- putStrLn $ "Created user: " ++ (BL.unpack $ getResponseBody response)
            selectChat user
    else exit

printAllChatOpponents :: User -> IO ()
printAllChatOpponents (User name _ chats) = do
    putStrLn "Select a chat or start a new chat by entering your friend's username:"
    forM_ chats $ \(Chat p1 p2 _) -> do
        putStrLn $ "|- " ++ (if p1 == name then p2 else p1)
    putStrLn "====================================================================================="

printAllMessages :: String -> Chat -> IO ()
printAllMessages username (Chat _ _ messages) = do
    forM_ messages $ \(Message fro to msg) -> do
        putStrLn $ (if fro == username then "You" else fro) ++ ": " ++ msg

selectChat :: User -> IO ()
selectChat user = do
    clearScreen
    putStrLn ("Welcome, "++(name user)++"!")
    printAllChatOpponents user
    tid <- forkIO $ do
        printSelectScreenLoop user
    waitForChatSelection user
    killThread tid
    targetChat <- getLine
    checkIfBackToMenu targetChat (updateAndDisplayChat user targetChat) user

printSelectScreenLoop :: User -> IO ()
printSelectScreenLoop user = do
    threadDelay 1000000
    request <- parseRequest ("http://localhost:3000/username/"++(username user))
    response <- httpLBS request
    let Just u = decode $ getResponseBody response
    if (getChatOppList user) == (getChatOppList u) then do
        printSelectScreenLoop u
    else do
        clearScreen
        putStrLn ("Welcome, "++(name user)++"!")
        putStrLn "Enter the username of the user you want to chat with: [enter '\\back' to go back to the menu]"
        printAllChatOpponents u
        printSelectScreenLoop u

waitForChatSelection :: User -> IO ()
waitForChatSelection user = do
    targetChat <- getLine
    selectChatIO user targetChat
    

selectChatIO :: User -> String -> IO ()
selectChatIO user targetChat = do
    putStrLn ("Selecting chat..."++targetChat++"..."++(username user))
    let chat = getChat (chats user) (username user) targetChat
    case chat of
        Just c -> do
            updateAndDisplayChat user targetChat
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
                    tid <- forkIO $ do
                        printMessagesLoop u p2 (Chat "" "" [])
                    waitForInput u p2
                    killThread tid
                Nothing -> do
                    putStrLn "Chat not found, create new chat?"
                    createNewChat u p2
        Nothing -> do
            putStrLn "Impossible"
            error "Impossible"

printMessagesLoop :: User -> String -> Chat -> IO ()
printMessagesLoop u p2 oldChat = do
    threadDelay 1000000
    let name = username u
    request <- parseRequest ("http://localhost:3000/username/"++name)
    response <- httpLBS request
    let Just user = decode $ getResponseBody response

    let newChat = getOrCreateChat (chats user) (username user) p2
    if not (areChatsEqual newChat oldChat) then do
        clearScreen
        printAllMessages name newChat
        putStrLn "Enter your message: [enter '\\back' to go back to the menu]"
        putStrLn "====================================================================================="
        printMessagesLoop user p2 newChat
    else printMessagesLoop user p2 newChat

waitForInput :: User -> String -> IO ()
waitForInput u target = do
    let name = username u
    msg <- getLine
    checkIfBackToMenu msg (sendMessageServer msg u target) u
    sendMessageServer msg u target
    waitForInput u target

checkUserExists :: String -> IO Bool
checkUserExists name = do
    request <- parseRequest ("http://localhost:3000/username/"++name)
    response <- httpLBS request
    -- putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
    let userM :: Maybe User = decode $ getResponseBody response
    -- putStrLn $ "User: " ++ (show userM)
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
                clearScreen
                sendMessageIO user targetUser
                updateAndDisplayChat user targetUser
            False -> do
                putStrLn "User not found, trying again..."
                putStrLn ""
                promptAndCreateNewChat user
    else selectChat user

promptAndCreateNewChat :: User -> IO ()
promptAndCreateNewChat user = do
    putStrLn "Enter the username of the user you want to chat with: [enter '\\back' to go back to the menu]"
    targetUser <- getLine
    checkIfBackToMenu targetUser (createNewChat user targetUser) user

sendMessageIO :: User -> String -> IO ()
sendMessageIO user targetUser = do
    putStrLn "Enter your message: [enter '\\back' to go back to the menu]"
    putStrLn "====================================================================================="
    msg <- getLine
    checkIfBackToMenu msg (sendMessageServer msg user targetUser) user

sendMessageServer :: String -> User -> String -> IO ()
sendMessageServer msg user target = do
    request <- parseRequest "POST http://localhost:3000/message"
    let message = Message (username user) target msg
    let req = setRequestBodyJSON message request
    response <- httpLBS req
    updateAndDisplayChat user target

checkIfBackToMenu :: String -> IO () -> User -> IO ()
checkIfBackToMenu input action user = do
    if input == "\\back" then selectChat user 
    else if input == "\\exit" then exit
    else action

loop :: IO ()
loop = do
    line <- getLine
    unless (line == "quit") $ do
        --request <- parseRequest "http://localhost:3000/ping"
        --response <- httpLBS request
        --putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
        putStrLn $ "You typed: " ++ line
        loop
