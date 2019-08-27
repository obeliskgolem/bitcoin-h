{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node where

import MathOP
import Data.Hashable
import Data.IORef

import Control.Monad.Reader
import Control.Monad


--  global data         --
nodeConfig :: String
nodeConfig = "node.config"

data Env = Env {
    eServerPort :: Int
    ,   eNodes :: IORef [Node]
    ,   eBlocks :: IORef [Block]
    ,   eTransactions :: IORef [Transaction]
    ,   eUTXO :: IORef [TxOutput]
} 

newtype AppT m a 
    = AppT 
    { unAppT :: ReaderT Env m a 
    } deriving (Functor, Applicative, Monad)


-- genesisBlockHeader :: BlockHeader


-- genesisBlock :: Block
-- genesisBlock = Block {mkHeader = genesis}

--  dealing with mutable data       --
initRegister :: (MonadReader Env m, MonadIO m) => m ()
initRegister = do
    -- env <- ask
    -- eServerPort env 
    liftIO $ putStrLn "registered"

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

    let env = Env {
        eServerPort = 19900
        , eNodes = nodes
        , eBlocks = blocks
        , eTransactions = tx
        , eUTXO = utxo
    }

    initRegister
