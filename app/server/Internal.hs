{-# LANGUAGE OverloadedStrings #-}

module Internal where

data State = State [User]
    deriving (Show)

-- A user is a person who can chat with other users.
-- Has a name, an ID, and a list of chats.
data User = User String String [Chat]
    deriving (Show)

instance Eq User where
    (User _ id1 _) == (User _ id2 _) = id1 == id2

username :: User -> String
username (User name _ _) = name

userId :: User -> String
userId (User _ id _) = id

-- A chat is a conversation between two users.
-- Has a fromID, toID, and a list of messages.
data Chat = Chat String String [Message]
    deriving (Show)
instance Eq Chat where
    (Chat user11 user12 _) == (Chat user21 user22 _) = (user11, user12) == (user21, user22)|| (user11, user12) == (user22, user21)

getChat :: [Chat] -> String -> String -> Chat
getChat [] fro to = Chat fro to []
getChat (c:cs) fro to = if (Chat fro to []) == c then c else getChat cs fro to

updateChatInList :: Chat -> [Chat] ->[Chat]
updateChatInList chat chats = chat : [c | c <- chats, c /= chat]

updateMessageInChat :: Message -> Chat -> Chat
updateMessageInChat msg (Chat from to msgs) = Chat from to (msg : msgs)

-- A message is a string sent from one user to another.
-- Has a fromID, a toID, and a string message.
data Message = Message String String String
    deriving (Show)

findUser :: [User] -> String -> Maybe User
findUser [] _ = Nothing
findUser (u:us) id = if u == (User "" id []) then Just u else findUser us id

updateUsersInList :: [User] -> [User] -> [User]
updateUsersInList users lou = users ++ [u | u <- lou, not (u `elem` users)]

updateUser :: User -> Message -> User
updateUser (User name id chats) (Message from to msg) = 
    User name id (updateChatInList (updateMessageInChat (Message from to msg) (getChat chats from to)) chats)

createUser :: State -> String -> String -> State
createUser (State users) name id = 
        State (User name id [] : users)

sendMsg :: State -> Message -> State
sendMsg (State users) (Message from to msg) = 
    let fromUser = findUser users from
        toUser = findUser users to
        in case (fromUser, toUser) of
            (Just fromU, Just toU) -> 
                State (updateUsersInList [updateUser fromU (Message from to msg), updateUser toU (Message from to msg)] users)
            _ -> State users
