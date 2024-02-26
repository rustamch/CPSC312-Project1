module Main where

import Control.Monad (unless)
import Network.HTTP.Simple
import Control.Monad
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    putStrLn "Type anything or 'quit' to exit:"
    loop

loop :: IO ()
loop = do
    line <- getLine
    unless (line == "quit") $ do
        --request <- parseRequest "http://localhost:3000/ping"
        --response <- httpLBS request
        --putStrLn $ "Response: " ++ (BL.unpack $ getResponseBody response)
        putStrLn $ "You typed: " ++ line
        loop