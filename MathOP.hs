module MathOP where

data IPAddress  = IPAddress String
data Port       = Int

data Node = Node {
    mkNodeID :: Int
    mkNodeAddr :: IPAddress
    mkNodePort :: Port
}

data Block = Block {
    mkTransactions :: [Transaction]
    mkMerkelTree :: MerkelTree
}

data BlockHeader = BlockHeader {
    mkPrevHeader :: Word
    mkTimestamp :: Word
    mkMerkelRoot :: Word
    mkNonce :: Word
}

data Transaction = Transaction {
    mkInputs :: [TxInput]
    mkOutputs :: [TxOutput]
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