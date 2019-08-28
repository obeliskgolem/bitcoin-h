{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MathOP where

import GHC.Generics
import Data.Aeson

import Servant
import Servant.API

import Data.Hashable

--  RESTful API             --
type ServerAPI =    "nodes" :> Get '[JSON] [Node] 
            :<|>    "register" :> ReqBody '[JSON] Node :> Post '[JSON] RegisterStatus

type NodeAPI    =   "blocks" :> Get '[JSON] [Block]

--  global data         --
difficulty :: Int
difficulty = maxBound :: Int

type HashV = Int

data Node = Node {
    mkNodeAddr :: String,
    mkNodePort :: Int
} deriving (Generic, Show, Eq)

instance ToJSON Node
instance FromJSON Node

data RegisterStatus =   RegisterSuccess
                    |   RegisterFailed
                    deriving (Generic, Show)

instance ToJSON RegisterStatus
instance FromJSON RegisterStatus

data Block = Block {
    mkHeader :: BlockHeader,
    mkTransactions :: [Transaction],
    mkMerkelTree :: MerkelTree
} deriving (Generic, Show)

instance ToJSON Block
instance FromJSON Block

data BlockHeader = BlockHeader {
    mkPrevHeader :: BlockHeader,
    mkMerkelRoot :: HashV,
    mkNonce :: Int
} deriving (Generic, Show)

instance ToJSON BlockHeader
instance FromJSON BlockHeader

data Transaction = Transaction {
    mkInputs :: [TxInput], 
    mkOutputs :: [TxOutput]
} deriving (Generic, Show)

instance ToJSON Transaction
instance FromJSON Transaction

data MerkelTree =   MerkelLeaf HashV
                |   MerkelBranch HashV (MerkelTree) (MerkelTree)
                deriving (Generic, Show)

instance ToJSON MerkelTree
instance FromJSON MerkelTree


data TxInput = TxInput {
    mkInAddr :: String,
    mkInAmount :: Int
} deriving (Generic, Show)

instance ToJSON TxInput
instance FromJSON TxInput

data TxOutput = TxOutput {
    mkOutAddr :: String,
    mkOutAmount :: Int
} deriving (Generic, Show)

instance ToJSON TxOutput
instance FromJSON TxOutput

instance Hashable BlockHeader
instance Hashable Transaction
instance Hashable MerkelTree
instance Hashable TxInput
instance Hashable TxOutput


--  global operations       --

qualified :: HashV -> Bool
qualified n = if n < difficulty && n > 0
    then True
    else False

qualifiedHash :: (Hashable a) => a -> Bool
qualifiedHash k = qualified $ hash k

--  pure blockchain functions   --
verifyBlock :: Block -> Bool
verifyBlock b = (merkelroot b == rootofmerkel (mkMerkelTree b)) && (qualifiedHash (mkHeader b))
    where
        merkelroot :: Block -> HashV
        merkelroot block = mkMerkelRoot (mkHeader block)

        rootofmerkel :: MerkelTree -> HashV
        rootofmerkel (MerkelBranch root a b) = root
        rootofmerkel (MerkelLeaf root) = root

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
