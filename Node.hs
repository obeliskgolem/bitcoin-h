{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Node where

import MathOP
-- import Data.Hashable
import Data.IORef
import Data.List

import Control.Monad.Reader
import Control.Monad

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

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

bServerURL = BaseUrl Http "localhost" 19900 ""

--  restful API as a node     --
type AppM = ReaderT Env Handler

mServer :: Env -> AppM a -> Handler a
mServer env server = runReaderT server env 

server1 :: ServerT NodeAPI AppM 
server1 =       serveBlocks
        :<|>    handlingNewNodes
        :<|>    handlingNewBlocks
        :<|>    handlingNewTransactions
    where
        serveBlocks :: AppM [Block]
        serveBlocks = do
            env <- ask
            blocks <- liftIO $ readIORef (eBlocks env)
            return blocks

        handlingNewNodes :: Node -> AppM RegisterStatus
        handlingNewNodes node = do
            env <- ask
            nodes <- liftIO $ readIORef (eNodes env)
            if node `elem` nodes
                then return RegisterFailed
                else do 
                    liftIO $ writeIORef (eNodes env) (node:nodes) 
                    return RegisterFailed

        handlingNewBlocks :: Block -> AppM RegisterStatus
        handlingNewBlocks block = do
            env <- ask
            blocks <- liftIO $ readIORef (eBlocks env)
            let newBlocks = blocks ++ (block:[])
            if verifyBlockChain newBlocks
                then do 
                    liftIO $ writeIORef (eBlocks env) newBlocks 
                    return RegisterSuccess
                else return RegisterFailed
                
        handlingNewTransactions :: Transaction -> AppM RegisterStatus
        handlingNewTransactions tx = do
            return RegisterFailed


app1 :: Env -> Application
app1 s = serve nodeApi $ hoistServer nodeApi (mServer s) server1

--  dealing with mutable data       --

--  register self and get nodes, server 
initRegister :: (MonadIO m) => ReaderT Env m ()
initRegister = do
    env <- ask
    manager' <- liftIO $ newManager defaultManagerSettings

    result1 <- liftIO $ runClientM (register (eSelf env)) ((mkClientEnv manager' bServerURL))
    liftIO $ print result1

    result2 <- liftIO $ runClientM getNodes ((mkClientEnv manager' bServerURL))
    case result2 of
        Left err    -> liftIO $ print err
        Right nodes -> liftIO $ writeIORef (eNodes env) nodes

--  try to get longest chain from other nodes
getBlockChain :: (MonadIO m) => ReaderT Env m ()
getBlockChain = do
    env <- ask
    nodes <- liftIO $ readIORef (eNodes env)
    blocks <- liftIO $ mapM getBlockChainVia nodes
    let validChains = filter verifyBlockChain blocks
    let lengths = maximum (map length validChains)
    let longest = find ((== lengths) . length) validChains

    case longest of
        Just chain  -> do
            liftIO $ writeIORef (eBlocks env) chain
            liftIO $ writeIORef (eUTXO env) (calcChainUTXO chain)
        Nothing     -> liftIO $ putStrLn "no valid chains found"
    where
        getBlockChainVia :: Node -> IO [Block]
        getBlockChainVia node = do
            manager' <- newManager defaultManagerSettings
            result1 <- runClientM getBlocks ((mkClientEnv manager' (BaseUrl Http (mkNodeAddr node) (mkNodePort node) ""))) 

            case result1 of
                Left err    -> do {putStrLn $ "Error while getting blocks: " ++ (show err); return []}
                Right b     -> return b

--  serving as node         --
runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)

    nodes <- newIORef ([] :: [Node])
    blocks <- newIORef ([genesisBlock])
    tx <- newIORef ([] :: [Transaction])
    utxo <- newIORef ([] :: [TxOutput])

    let env = Env 19900 (Node "localhost" port) nodes blocks tx utxo

    runReaderT initRegister env
    runReaderT getBlockChain env

    run port (app1 env)
