{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/ping" $ do
    text "pong"
