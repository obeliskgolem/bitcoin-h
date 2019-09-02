{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Node where

import MathOP

import Data.IORef
import Data.List
import Data.Either

import Control.Monad.Reader
import Control.Monad
import Control.Concurrent

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
                    liftIO $ writeIORef (eTransactions env) []
                    return RegisterSuccess

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
            env <- ask
            utxo <- liftIO $ readIORef (eUTXO env) 
            trans <- liftIO $ readIORef (eTransactions env)
            if (verifyTransaction tx utxo) && (isRight (calcUTXO utxo (tx:trans)))
                then do
                    liftIO $ writeIORef (eTransactions env) (tx:trans)
                    return RegisterSuccess
                else return RegisterFailed


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
    allNodes <- liftIO $ readIORef (eNodes env)
    let nodes = delete (eSelf env) allNodes
    blocks <- liftIO $ mapM getBlockChainVia nodes
    let validChains = filter verifyBlockChain blocks
    let validTxChains = filter (isRight . (calcChainUTXO [])) validChains
    let lengths = maximum (map length validTxChains)
    let longest = find ((== lengths) . length) validTxChains

    case longest of
        Just chain  -> do
            liftIO $ writeIORef (eBlocks env) chain
            liftIO $ writeIORef (eUTXO env) (fromRight [] (calcChainUTXO [] chain))
        Nothing     -> liftIO $ putStrLn "no valid chains found"
    where
        getBlockChainVia :: Node -> IO [Block]
        getBlockChainVia node = do
            manager' <- newManager defaultManagerSettings
            result1 <- runClientM getBlocks ((mkClientEnv manager' (BaseUrl Http (mkNodeAddr node) (mkNodePort node) ""))) 

            case result1 of
                Left err    -> do {putStrLn $ "Error while getting blocks: " ++ (show err); return []}
                Right b     -> return b

--  mine a block with current transactions
mineBlock :: (MonadIO m) => ReaderT Env m ()
mineBlock = do
    env <- ask
    nodes <- liftIO $ readIORef (eNodes env)
    trans <- liftIO $ readIORef (eTransactions env)
    blocks <- liftIO $ readIORef (eBlocks env)
    utxo <- liftIO $ readIORef (eUTXO env)
    when (trans /= [] && blocks /= []) $ do
        let t_tree = generateMerkelTree trans
        let t_header = mining (mkHeader (last blocks)) (getMerkelRoot t_tree) 1
        let t_block = Block t_header trans t_tree
        liftIO $ writeIORef (eBlocks env) (blocks ++ [t_block])
        liftIO $ writeIORef (eUTXO env) (fromRight [] (calcUTXO utxo trans))
        liftIO $ writeIORef (eTransactions env) []
        mapM_ (broadcastBlock t_block) nodes
        return ()

--  mine a block with current transactions
broadcastBlock:: (MonadIO m) => Block -> Node -> ReaderT Env m ()
broadcastBlock newBlock node = do
    env <- ask
    when (node /= (eSelf env)) $ do
        manager' <- liftIO $ newManager defaultManagerSettings
        liftIO $ runClientM (updateBlocks newBlock) ((mkClientEnv manager' (makeBaseURL node)))
        return ()

--  serving as node         --
runNode :: Int -> IO ()
runNode port  = do
    nodes <- newIORef ([] :: [Node])
    blocks <- newIORef ([genesisBlock])
    tx <- newIORef ([] :: [Transaction])
    utxo <- newIORef (mkOutputs (head genesisTransaction))

    let env = Env 19900 (Node "localhost" port) nodes blocks tx utxo

    runReaderT initRegister env
    runReaderT getBlockChain env

    forkIO $ forever $ do
        threadDelay $ 1000000 * 30 
        runReaderT mineBlock env

    putStrLn $ "bitcoin-h node running at port " ++ (show port)

    run port (app1 env)
