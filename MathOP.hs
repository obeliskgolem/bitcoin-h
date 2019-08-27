{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MathOP where

import GHC.Generics
import Data.Aeson

-- import Crypto.Hash.MD5
import Data.Hashable
import Data.ByteString.UTF8

-- data IPAddress  = IPAddress String deriving (Generic, Show)
-- data Port       = Int deriving (Generic, Show)

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

data BlockHeader = BlockHeader {
    mkPrevHeader :: BlockHeader,
    mkMerkelRoot :: HashV,
    mkNonce :: Int
} deriving (Generic, Show)

instance Hashable BlockHeader

data Transaction = Transaction {
    mkInputs :: [TxInput], 
    mkOutputs :: [TxOutput]
} deriving (Generic, Show)

instance Hashable Transaction

data MerkelTree =   MerkelLeaf HashV
                |   MerkelBranch HashV (MerkelTree) (MerkelTree)
                deriving (Generic, Show)

instance Hashable MerkelTree

data TxInput = TxInput {
    mkInAddr :: String,
    mkInAmount :: Int
} deriving (Generic, Show)

instance Hashable TxInput

data TxOutput = TxOutput {
    mkOutAddr :: String,
    mkOutAmount :: Int
} deriving (Generic, Show)

instance Hashable TxOutput


--  global operations       --

qualified :: HashV -> Bool
qualified n = if n < difficulty && n > 0
    then True
    else False

qualifiedHash :: (Hashable a) => a -> Bool
qualifiedHash k = qualified $ hash k