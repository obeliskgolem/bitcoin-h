{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP

--  global data         --
nodeConfig :: String
nodeConfig = "node.config"

difficulty :: Word
difficulty = 0x00fffffffffffffff

-- genesisBlockHeader :: BlockHeader


-- genesisBlock :: Block
-- genesisBlock = Block {mkHeader = genesis}

--  test data           --


--  date kept in a node --
-- nodeList :: IORef [Node]

-- blockList :: IORef [Block]

-- headerList :: IORef [BlockHeader]

-- incommingTransaction :: IORef [Transaction]

--  internal operations --
-- initRegister :: IO ()

verifyBlock :: Block -> Bool
verifyBlock b = (merkelroot b == rootofmerkel (mkMerkelTree b)) && (mkhash (mkHeader b) < difficulty)
    where
        merkelroot :: Block -> Word
        merkelroot block = mkMerkelRoot (mkHeader block)

        rootofmerkel :: MerkelTree -> Word
        rootofmerkel (MerkelBranch root a b) = root
        rootofmerkel _ = 0

mining :: BlockHeader -> Word -> Word -> BlockHeader
mining phead mroot nonce = if calculate 
    then constructBH 
    else mining phead mroot (nonce + 1)
    where 
        constructBH :: BlockHeader
        constructBH = BlockHeader {mkPrevHeader = phead, mkMerkelRoot = mroot, mkNonce = nonce}

        calculate :: Bool
        calculate = (mkhash constructBH) < difficulty


-- verifyTransaction :: Transaction -> Bool

-- requestBlockViaHeader :: BlockHeader -> IO ()

-- requestBlock :: Node -> IO ()

-- requestFullChain :: Node -> IO ()

-- respondRequest :: Node -> IO ()

-- broadcastNode :: [Node] -> IO ()




runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)
