{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import MathOP
import Servant
import Servant.API

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Data.IORef
import Data.Aeson
import GHC.Generics

import Control.Monad.Trans
import Control.Concurrent

server1 :: IORef [Node] -> Server ServerAPI
server1 nodeList =      listnode nodeList
                :<|>    register nodeList

    where   
        listnode :: IORef [Node] -> Handler [Node]
        listnode nodeList = do
            list <- liftIO $ readIORef nodeList
            liftIO $ putStrLn (show list)
            return list

        register :: IORef [Node] -> Node -> Handler RegisterStatus
        register nodeList node = do
            list <- liftIO $ readIORef nodeList 
            let s = if (node `elem` list) then list else (node:list)
            liftIO $ putStrLn (show s)
            liftIO $ writeIORef nodeList s
            if (node `elem` s) then 
                return RegisterSuccess
            else
                return RegisterFailed

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app1 :: IORef [Node] -> Application
app1 nodeList = serve serverAPI (server1 nodeList)

runServer :: Int -> IO ()
runServer port  = do
    putStrLn $ "bitcoin-h server api running at port " ++ (show port)
    n <- newIORef []
    run port (app1 n)
