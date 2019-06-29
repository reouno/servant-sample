module Main where

import Network.Wai.Handler.Warp (run)

import Server (app1)

main :: IO ()
main = run 8081 app1
