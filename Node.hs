{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Node where

import MathOP
import Data.Hashable
import Data.IORef

import Control.Monad.Reader
import Control.Monad

import Servant
import Servant.API
import Servant.Client

import Data.Aeson

import Network.HTTP.Client          (newManager, defaultManagerSettings)

--  global definitions          --
data Env = Env {
    eServerPort :: Int
    ,   eSelf :: Node
    ,   eNodes :: IORef [Node]
    ,   eBlocks :: IORef [Block]
    ,   eTransactions :: IORef [Transaction]
    ,   eUTXO :: IORef [TxOutput]
} 

--  restful API as a client     --
api :: Proxy ServerAPI
api = Proxy

getNodes :: ClientM [Node]
register :: Node -> ClientM RegisterStatus

getNodes :<|> register = client api

-- genesisBlockHeader :: BlockHeader


-- genesisBlock :: Block
-- genesisBlock = Block {mkHeader = genesis}

--  dealing with mutable data       --
initRegister :: (MonadIO m) => ReaderT Env m ()
initRegister = do
    env <- ask
    manager' <- liftIO $ newManager defaultManagerSettings
    result1 <- liftIO $ runClientM (register (eSelf env)) ((mkClientEnv manager' (BaseUrl Http "localhost" 19900 "")))
    result2 <- liftIO $ runClientM getNodes ((mkClientEnv manager' (BaseUrl Http "localhost" 19900 "")))
    liftIO $ print result1
    liftIO $ print result2

-- respondRequest :: Node -> IO ()

-- broadcastNode :: [Node] -> IO ()


--  serving as node         --
runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)

    nodes <- newIORef ([] :: [Node])
    blocks <- newIORef ([] :: [Block])
    tx <- newIORef ([] :: [Transaction])
    utxo <- newIORef ([] :: [TxOutput])

    let env = Env 19900 (Node "localhost" port) nodes blocks tx utxo

    runReaderT initRegister env
