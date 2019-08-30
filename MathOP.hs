{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MathOP where

import GHC.Generics
import Data.Aeson

import Servant
import Servant.API
import Servant.Client

import Data.Hashable

--  RESTful API             --
type ServerAPI =    "nodes" :> Get '[JSON] [Node] 
            :<|>    "register" :> ReqBody '[JSON] Node :> Post '[JSON] RegisterStatus

type NodeAPI    =   "blocks" :> Get '[JSON] [Block]
            :<|>    "updateNode" :> ReqBody '[JSON] Node :> Post '[JSON] RegisterStatus
            :<|>    "updateBlocks" :> ReqBody '[JSON] Block :> Post '[JSON] RegisterStatus

serverApi :: Proxy ServerAPI
serverApi = Proxy

getNodes :: ClientM [Node]
register :: Node -> ClientM RegisterStatus

getNodes :<|> register = client serverApi

nodeApi :: Proxy NodeAPI
nodeApi = Proxy

getBlocks :: ClientM [Block]
updateNodes :: Node -> ClientM RegisterStatus
updateBlocks :: Node -> ClientM RegisterStatus
getBlocks :<|> updateNodes :<|> updateBlocks = client nodeApi

--  global data         --
type HashV = Int

difficulty :: HashV
difficulty =            0003372036854775807                
-- maxBount :: Int ==   9223372036854775807

genesisTransaction = [Transaction [] [TxOutput "Guangzhou" 100, TxOutput "shanghai" 100, TxOutput "Beijing" 100]]
genesisMerkelTree = generateMerkelTree genesisTransaction
genesisHeader = ChainedBlockHeader Genesis (getMerkelRoot genesisMerkelTree) 9675
genesisBlock = Block genesisHeader genesisTransaction genesisMerkelTree


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
} deriving (Generic, Show, Eq)

instance ToJSON Block
instance FromJSON Block

data BlockHeader =  Genesis
                |   ChainedBlockHeader {
                        mkPrevHeader :: BlockHeader,
                        mkMerkelRoot :: HashV,
                        mkNonce :: Int
                    } deriving (Generic, Show, Eq)



instance ToJSON BlockHeader
instance FromJSON BlockHeader

data Transaction = Transaction {
    mkInputs :: [TxInput], 
    mkOutputs :: [TxOutput]
} deriving (Generic, Show, Eq)

instance ToJSON Transaction
instance FromJSON Transaction

data MerkelTree =   MerkelLeaf HashV
                |   MerkelBranch HashV (MerkelTree) (MerkelTree)
                deriving (Generic, Show, Eq)

instance ToJSON MerkelTree
instance FromJSON MerkelTree


data TxInput = TxInput {
    mkInAddr :: String,
    mkInAmount :: Int
} deriving (Generic, Show, Eq)

instance ToJSON TxInput
instance FromJSON TxInput

data TxOutput = TxOutput {
    mkOutAddr :: String,
    mkOutAmount :: Int
} deriving (Generic, Show, Eq)

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

qualifiedHash :: (Hashable a, Show a) => a -> Bool
qualifiedHash k = qualified $ hash $ show $ hash $ show k

--  pure blockchain functions   --
getMerkelRoot :: MerkelTree -> HashV
getMerkelRoot (MerkelLeaf a) = a
getMerkelRoot (MerkelBranch h _ _) = h

verifyBlock :: Block -> Bool
verifyBlock b = (merkelroot b == getMerkelRoot (mkMerkelTree b)) && (qualifiedHash (mkHeader b))
    where
        merkelroot :: Block -> HashV
        merkelroot block = mkMerkelRoot (mkHeader block)

verifyBlockChain :: [Block] -> Bool
verifyBlockChain bs = (foldl (&&) True (map verifyBlock bs)) && linked (reverse bs)
        where
            linked :: [Block] -> Bool
            linked (a:b:xs)     = (mkPrevHeader (mkHeader a) == mkHeader b) && linked xs
            linked (a:[])       = a == genesisBlock
            linked []           = True

mining :: BlockHeader -> HashV -> Int -> BlockHeader
mining phead mroot nonce = if calculate 
    then constructBH 
    else mining phead mroot (nonce + 1)
    where 
        constructBH :: BlockHeader
        constructBH = ChainedBlockHeader {mkPrevHeader = phead, mkMerkelRoot = mroot, mkNonce = nonce}

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
