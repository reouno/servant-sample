module Main where

import           Network.Wai.Handler.Warp       ( run )

import           Server                         ( app1
                                                , app10MatrixUsers
                                                , app11MatrixUsers
                                                , app12Products
                                                , app13UsersProds
                                                , app14Reader
                                                , app2
                                                , app3
                                                , app4
                                                , app5
                                                , app6
                                                , app7
                                                , app8
                                                , app8'
                                                , app9
                                                )

main :: IO ()
--main = run 8081 app1
--main = run 8081 app2
--main = run 8081 app3
--main = run 8081 app4
--main = run 8081 app5
--main = run 8081 app6
--main = run 8081 app7
--main = run 8081 app8
--main = run 8081 app8'
--main = run 8081 app9
--main = run 8081 app10MatrixUsers
--main = run 8081 app11MatrixUsers
--main = run 8081 app12Products
--main = run 8081 app13UsersProds
main = run 8081 app14Reader
