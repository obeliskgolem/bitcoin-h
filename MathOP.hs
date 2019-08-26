{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MathOP where

import GHC.Generics
import Data.Aeson

import Crypto.Hash.MD5
import Data.ByteString.UTF8

-- data IPAddress  = IPAddress String deriving (Generic, Show)
-- data Port       = Int deriving (Generic, Show)

data Node = Node {
    -- mkNodeID :: Int,
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
    mkPrevHeader :: Word,
    mkTimestamp :: Word,
    mkMerkelRoot :: Word,
    mkNonce :: Word
} deriving (Generic, Show)

data Transaction = Transaction {
    mkInputs :: [TxInput], 
    mkOutputs :: [TxOutput], 
    mkSig :: Word
} deriving (Generic, Show)

data MerkelTree =   MerkelLeaf Word
                |   MerkelBranch Word (MerkelTree) (MerkelTree)
                deriving (Generic, Show)

data TxInput = TxInput {
    mkInAddr :: Word
} deriving (Generic, Show)

data TxOutput = TxOutput {
    mkOutAddr :: Word
} deriving (Generic, Show)

mkhash :: (Show a) => a -> Word
mkhash = read . toString . hash . fromString . show