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
import Data.List

--  RESTful API             --
type ServerAPI =    "nodes" :> Get '[JSON] [Node] 
            :<|>    "register" :> ReqBody '[JSON] Node :> Post '[JSON] RequestStatus
            :<|>    "newTransaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] RequestStatus

type NodeAPI    =   "blocks" :> Get '[JSON] [Block]
            :<|>    "updateNodes" :> ReqBody '[JSON] Node :> Post '[JSON] RequestStatus
            :<|>    "updateBlocks" :> ReqBody '[JSON] Block :> Post '[JSON] RequestStatus
            :<|>    "newServerTransaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] RequestStatus

serverApi :: Proxy ServerAPI
serverApi = Proxy

getNodes :: ClientM [Node]
register :: Node -> ClientM RequestStatus
newTransaction :: Transaction -> ClientM RequestStatus

getNodes :<|> register :<|> newTransaction = client serverApi

nodeApi :: Proxy NodeAPI
nodeApi = Proxy

getBlocks :: ClientM [Block]
updateNodes :: Node -> ClientM RequestStatus
updateBlocks :: Block -> ClientM RequestStatus
newServerTransaction :: Transaction -> ClientM RequestStatus
getBlocks :<|> updateNodes :<|> updateBlocks :<|> newServerTransaction = client nodeApi

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

data RequestStatus =   RequestSuccess
                    |   RequestFailed
                    deriving (Generic, Show)

instance ToJSON RequestStatus
instance FromJSON RequestStatus

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

eqTx :: TxInput -> TxOutput -> Bool
eqTx txin txout = (mkInAddr txin == mkOutAddr txout) && (mkInAmount txin == mkOutAmount txout)

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
verifyTransaction tx@Transaction{mkInputs=txin, mkOutputs=txout} utxo = checkUTXO && checkAmount
    where
        checkAmount = verifyTransactionAmount tx

        checkUTXO = case (calcUTXO utxo [tx]) of
            Left _  -> False
            _       -> True
        
verifyTransactionAmount :: Transaction -> Bool
verifyTransactionAmount tx = sum (map mkInAmount $ mkInputs tx) >= sum (map mkOutAmount $ mkOutputs tx)

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


calcChainUTXO :: [TxOutput] -> [Block] -> Either NotValidTransaction [TxOutput]
calcChainUTXO utxo [] = Right utxo
calcChainUTXO utxo (b:blocks)   | b == genesisBlock = Right (foldl (++) [] (map (mkOutputs) (mkTransactions genesisBlock)))
                                | otherwise         = (calcUTXO utxo (mkTransactions b)) >>= (`calcChainUTXO` blocks)

data NotValidTransaction = NotValidTransaction deriving (Generic, Show)

calcUTXO :: [TxOutput] -> [Transaction] -> Either NotValidTransaction [TxOutput]
calcUTXO txout []           = Right txout
calcUTXO prev_utxo (t:tx)   = (prev_utxo `solveTx` t) >>= (`calcUTXO` tx)
    where
        solveTx :: [TxOutput] -> Transaction -> Either NotValidTransaction [TxOutput]
        solveTx utxo incoming = (utxo `delBy` (mkInputs incoming)) >>= (`plusBy` incoming)

        delBy :: [TxOutput] -> [TxInput] -> Either NotValidTransaction [TxOutput]
        delBy utxo [] = Right utxo
        delBy utxo (t:txin) = case (find (t `eqTx`) utxo) of
            Just a  -> delBy (delete a utxo) txin
            Nothing -> Left NotValidTransaction

        plusBy :: [TxOutput] -> Transaction -> Either NotValidTransaction [TxOutput]
        plusBy utxo incoming = Right $ utxo ++ (mkOutputs incoming)
        
--  miscellaneous
makeBaseURL :: Node -> BaseUrl
makeBaseURL node = BaseUrl Http (mkNodeAddr node) (mkNodePort node) ""