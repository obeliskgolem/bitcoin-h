{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import MathOP
import Servant
import Servant.API

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)


type NodeAPI = "nodes" :> Get '[JSON] [Node]

type RegisterAPI = "register" :> ReqBody '[JSON] Node :> Get '[JSON] Node

nodes :: IORef [Node]

server1 :: [Node] -> Server NodeAPI
server1 = return

server2 :: Server NodeAPI
server2 = return 

nodesAPI :: Proxy NodeAPI
nodesAPI = Proxy

registerAPI :: Proxy RegisterAPI
registerAPI = Proxy

app1 :: Application
app1 = serve nodeAPI (server1 nodes)

app2 :: Application
app2 = serve registerAPI server2

runServer :: Int -> IO ()
runServer port  = do
    putStrLn $ "bitcoin-h server running at port " ++ (show port)
    nodes <- newIORef ([] :: [Node])
    n <- readIORef nodes
    run port $ serve nodeAPI (server1 n)
    run port app2