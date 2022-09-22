module Main where

import Chat (runServer)
import Prelude

main :: IO ()
main = Chat.runServer
