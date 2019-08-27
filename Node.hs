{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP

--  global data         --
nodeConfig :: String
nodeConfig = "node.config"


-- genesisBlockHeader :: BlockHeader


-- genesisBlock :: Block
-- genesisBlock = Block {mkHeader = genesis}

--  test data           --


--  date kept in a node --
-- nodeList :: IORef [Node]

-- blockList :: IORef [Block]

-- incommingTransaction :: IORef [Transaction]

-- unspentMoney :: IORef [TxOutput]

--  internal operations --
-- initRegister :: IO ()

verifyBlock :: Block -> Bool
verifyBlock b = (merkelroot b == rootofmerkel (mkMerkelTree b)) && (qualifiedHash (mkHeader b))
    where
        merkelroot :: Block -> HashV
        merkelroot block = mkMerkelRoot (mkHeader block)

        rootofmerkel :: MerkelTree -> HashV
        rootofmerkel (MerkelBranch root a b) = root
        rootofmerkel _ = 0

mining :: BlockHeader -> HashV -> Int -> BlockHeader
mining phead mroot nonce = if calculate 
    then constructBH 
    else mining phead mroot (nonce + 1)
    where 
        constructBH :: BlockHeader
        constructBH = BlockHeader {mkPrevHeader = phead, mkMerkelRoot = mroot, mkNonce = nonce}

        calculate :: Bool
        calculate = qualifiedHash constructBH 

verifyTransaction :: Transaction -> [TxOutput] -> Bool
verifyTransaction Transaction{mkInputs=txin, mkOutputs=txout} utxo = checkUTXO && checkAmount
    where
        checkAmount = ((foldl (+) 0 (map mkInAmount txin)) - (foldl (+) 0 (map mkOutAmount txout))) >= 0

        checkUTXO = foldl (&&) True (map (`seekUTXO` utxo) txin)

        seekUTXO :: TxInput -> [TxOutput] -> Bool
        seekUTXO tin (x:xs) = if (mkInAddr tin == mkOutAddr x) && (mkInAmount tin == mkOutAmount x)
            then True
            else seekUTXO tin xs
        seekUTXO _ [] = False

-- generateMerkelTree :: [Transaction] -> MerkelTree
-- generateMerkelTree tx = genMTree (map (MerkelLeaf . hash) tx)
--     where
--         genMTree :: [MerkelTree] -> MerkelTree
--         genMTree (x:[]) = x
--         genMTree 

-- requestBlockViaHeader :: BlockHeader -> IO ()

-- requestBlock :: Node -> IO ()

-- requestFullChain :: Node -> IO ()

-- respondRequest :: Node -> IO ()

-- broadcastNode :: [Node] -> IO ()




runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)
