{-# LANGUAGE OverloadedStrings #-}

module Node where

import MathOP
import Data.Hashable


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


--  pure functions          --
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

generateMerkelTree :: [Transaction] -> MerkelTree
generateMerkelTree tx = genMTree (map (MerkelLeaf . hash) tx)
    where
        genMTree :: [MerkelTree] -> MerkelTree
        genMTree [] = MerkelLeaf (hash ([] :: [MerkelTree]))
        genMTree (x:[]) = x
        genMTree xs = genMTree (genPairs xs)

        genPairs :: [MerkelTree] -> [MerkelTree]
        genPairs (a:b:xs) = (MerkelBranch (hash (hash a + hash b)) a b : genPairs xs)
        genPairs (x:[]) = [MerkelBranch (hash (hash x + hash x)) x x]
        genPairs [] = []

--  dealing with mutable data       --
--  initRegister :: IO ()

requestBlock :: [Node] -> IO ()
requestBlock nodes = do
    

-- requestFullChain :: Node -> IO ()

-- respondRequest :: Node -> IO ()

-- broadcastNode :: [Node] -> IO ()

--  test data               --
testInput1 = TxInput{mkInAddr = "Shanghai", mkInAmount = 10}
testInput2 = TxInput{mkInAddr = "Guangzhou", mkInAmount = 20}
testInput3 = TxInput{mkInAddr = "Beijing", mkInAmount = 50}

testOutput1 = TxOutput{mkOutAddr = "Shanghai", mkOutAmount = 10}
testOutput2 = TxOutput{mkOutAddr = "Beijing", mkOutAmount = 20}
testOutput3 = TxOutput{mkOutAddr = "Guangzhou", mkOutAmount = 30}

testTransaction =   [Transaction{mkInputs = [testInput1], mkOutputs = [testOutput1]}
                    , Transaction{mkInputs = [testInput2], mkOutputs = [testOutput2]}
                    , Transaction{mkInputs = [testInput3], mkOutputs = [testOutput3]}
                    ]

--  serving as node         --
runNode :: Int -> IO ()
runNode port  = do
    putStrLn $ "bitcoin-h node running at port " ++ (show port)
