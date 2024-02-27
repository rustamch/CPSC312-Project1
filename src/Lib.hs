{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where


import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data State = State {
    user :: [User]
} deriving (Show, Generic)

instance ToJSON State
instance FromJSON State

-- A user is a person who can chat with other users.
-- Has a name, an ID, and a list of chats.
data User = User {
    name :: String,
    id :: String,
    chats :: [Chat]
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance Eq User where
    (User _ id1 _) == (User _ id2 _) = id1 == id2

areChatsEqual :: Chat -> Chat -> Bool
areChatsEqual (Chat p1 p2 m1) (Chat p3 p4 m2) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
    && m1 == m2


username :: User -> String
username (User name _ _) = name

userId :: User -> String
userId (User _ id _) = id

userChats :: User -> [Chat]
userChats (User _ _ chats) = chats

findUserById :: [User] -> String -> Maybe User
findUserById [] _ = Nothing
findUserById (u:us) id = if u == (User "" id []) then Just u else findUserById us id

findUserByName :: [User] -> String -> Maybe User
findUserByName [] _ = Nothing
findUserByName (u:us) name = if (username u) == name then Just u else findUserByName us name

-- A chat is a conversation between two users.
-- Has a fromID, toID, and a list of messages.
data Chat = Chat {
    p1 :: String,
    p2 :: String,
    messages :: [Message]
} deriving (Show, Generic)

instance ToJSON Chat
instance FromJSON Chat

instance Eq Chat where
    (Chat user11 user12 _) == (Chat user21 user22 _) = (user11, user12) == (user21, user22)|| (user11, user12) == (user22, user21)

getChat :: [Chat] -> String -> String -> Maybe Chat
getChat [] fro to = Nothing
getChat (c:cs) fro to = if (Chat fro to []) == c then Just c else getChat cs fro to

getOrCreateChat :: [Chat] -> String -> String -> Chat
getOrCreateChat chats fro to =
    case getChat chats fro to of
        Just c -> c
        _ -> Chat fro to []

getUsers :: State -> [User]
getUsers (State users) = users

getUserById :: State -> String -> Maybe User
getUserById (State users) id = findUserById users id

getUserByName :: State -> String -> Maybe User
getUserByName (State users) name = findUserByName users name

-- changeName :: State -> String -> String -> State
-- changeName (State users) id name =
--     let user = findUser users id
--         in case user of
--             Just (User _ id chats) -> State (updateUsersInList [User name id chats] users)
--             _ -> State users

updateChatInList :: Chat -> [Chat] ->[Chat]
updateChatInList chat chats = chat : [c | c <- chats, c /= chat]

updateMessageInChat :: Message -> Chat -> Chat
updateMessageInChat msg (Chat from to msgs) = Chat from to (msg : msgs)

-- A message is a string sent from one user to another.
-- Has a fromID, a toID, and a string message.
data Message = Message {
    from :: String,
    to :: String,
    msg :: String
}
    deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

instance Eq Message where
    (Message from1 to1 msg1) == (Message from2 to2 msg2) = (from1, to1, msg1) == (from2, to2, msg2)


updateUsersInList :: [User] -> [User] -> [User]
updateUsersInList users lou = users ++ [u | u <- lou, not (u `elem` users)]

updateUser :: User -> Message -> User
updateUser (User name id chats) (Message from to msg) =
    User name id (updateChatInList (updateMessageInChat (Message from to msg) (getOrCreateChat chats from to)) chats)

createUser :: State -> String -> String -> State
createUser (State users) name id =
    let existingUser1 = findUserById users id
        existingUser2 = findUserByName users name
        in case (existingUser1, existingUser2) of
            (Nothing, Nothing) -> State (User name id [] : users)
            _ -> State users

sendMsg :: State -> Message -> State
sendMsg (State users) (Message fromName toName msg) =
    let fromUser = findUserByName users fromName
        toUser = findUserByName users toName
        in case (fromUser, toUser) of
            (Just fromU, Just toU) ->
                State (updateUsersInList [updateUser fromU (Message fromName toName msg), updateUser toU (Message fromName toName msg)] users)
            _ -> State users

getChats :: State -> String -> [Chat]
getChats (State users) id =
    let user = findUserById users id
        in case user of
            Just (User _ _ chats) -> chats
            _ -> []
