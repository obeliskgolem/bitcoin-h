{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP

--  date kept in a node --
nodeList :: IORef [Node]

blockList :: IORef [Block]

headerList :: IORef [BlockHeader]

incommingTransaction :: IORef [Transaction]

--  internal operations --
initRegister :: IO ()

verifyBlock :: Block -> Bool

verifyTransaction :: Transaction -> Bool

requestBlockViaHeader :: BlockHeader -> IO ()

requestBlock :: Node -> IO ()

requestFullChain :: Node -> IO ()

respondRequest :: Node -> IO ()

broadcastNode :: [Node] -> IO ()




runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)
