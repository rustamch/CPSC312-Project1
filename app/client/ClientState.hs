module ClientState where

import Lib (User)

-- ClientState is one of 
    -- LoginState
    -- ChatState
    -- MainMenuState
data ClientState = LoginState String 
                    | ChatState User String
                    | MainMenuState User 
                    deriving (Show, Eq)