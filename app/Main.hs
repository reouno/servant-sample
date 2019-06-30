module Main where

import Network.Wai.Handler.Warp (run)

import Server (app1, app2, app3, app4)

main :: IO ()
--main = run 8081 app1
--main = run 8081 app2
--main = run 8081 app3
main = run 8081 app4
