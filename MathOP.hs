{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MathOP where

import GHC.Generics
import Data.Aeson

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
    mkTransactions :: [Transaction],
    mkMerkelTree :: MerkelTree
}

data BlockHeader = BlockHeader {
    mkPrevHeader :: Word,
    mkTimestamp :: Word,
    mkMerkelRoot :: Word,
    mkNonce :: Word
}

data Transaction = Transaction {
    mkInputs :: [TxInput], 
    mkOutputs :: [TxOutput], 
    mkSig :: Word
}

data MerkelTree =   MerkelLeaf Word
                |   MerkelBranch Word (MerkelTree) (MerkelTree)

data TxInput = TxInput {
    mkInAddr :: Word
}

data TxOutput = TxOutput {
    mkOutAddr :: Word
}