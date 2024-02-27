module RemoteCall where

import Lib 
import Network.HTTP.Simple

createNewUser :: String -> String -> IO String
createNewUser username userid= do
    request <- parseRequest "POST http://localhost:3000/user"
    let user = User username userid []
    let req = setRequestBodyJSON user request
    response <- httpLBS req
    return $ getResponseBody response

