# bitcoin-h

This simple project aims to implement the bitcoin system as a proof-of-concept.

Depends heavily on the [Servant](http://hackage.haskell.org/package/servant) library to provide RESTful APIs for node/node, node/server communication

## The Design

Since a bitcoin-h node must maintain a running state of the whole blockchain, [the ReaderT pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) is mainly applied in the node codes.

Several important bitcoin concepts are **not** implemented here, including:
- [ ] bitcoin fork
- [ ] pay-to-public-key-hash, as well as other transaction validation methods
- [ ] mining incentives

Implemented concepts:
- [x] Server/Node communication via web services
- [x] Node register, block chain requesting
- [x] transaction mining, new block broadcasting
- [x] Merkel tree generation and validation
- [x] a static computation difficulty
- [x] other related stuff

## Usage

For mocking bitcoin-h server, run
``` bash
$ cabal new-build
$ cabal new-run
```

For mocking bitcoin-h nodes, run
``` bash
$ cabal new-repl
*Server> import Node

*Server Node> forkIO $ runNode 19901
*Server Node> forkIO $ runNode 19902
...
```

## APIs

API | Method | Endpoints | Parameters | Return
----| -------| --------- | ------------- | --- |
Server | GET | /nodes | | [Nodes]
Server | POST | /register | Node | Status
Server | POST | /newTransaction | Transaction | Status
Node | GET | /blocks | | [Blocks]
Node | POST | /updateNodes | Node | Status
Node | POST | /updateBlocks | Block | Status
Node | POST | /newServerTransaction | Transaction | Status

## Examples

_Supposing that server is running on port 19900, two nodes on ports 19901/19902_

List all the nodes
``` bash
$ curl --header "Content-Type: application/json" --request POST --data '{"mkInputs":[{"mkInAddr":"Shanghai","mkInAmount":40}],"mkOutputs":[{"mkOutAddr":"Guangzhou","mkOutAmount":10}, {"mkOutAddr":"Shanghai","mkOutAmount":30}]}' http://localhost:19900/newTransaction

"RequestSuccess"%
```

Show the GenesisBlock

``` bash
$ curl --header "Content-Type: application/json" --request GET http://localhost:19901/blocks

[{"mkHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelTree":{"tag":"MerkelLeaf","contents":7694568972639932276},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":100,"mkOutAddr":"Guangzhou"},{"mkOutAmount":100,"mkOutAddr":"shanghai"},{"mkOutAmount":100,"mkOutAddr":"Beijing"}],"mkInputs":[]}]}]%
```

New Transaction
``` bash
$ curl --header "Content-Type: application/json" --request POST --data '{"mkInputs":[{"mkInAddr":"Guangzhou","mkInAmount":100}],"mkOutputs":[{"mkOutAddr":"Guangzhou","mkOutAmount":50}, {"mkOutAddr":"Shanghai","mkOutAmount":40}]}' http://localhost:19900/newTransaction

"RequestSuccess"%
```

Show the mined blocks
``` bash
$ curl --header "Content-Type: application/json" --request GET http://localhost:19902/blocks

[{"mkHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelTree":{"tag":"MerkelLeaf","contents":7694568972639932276},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":100,"mkOutAddr":"Guangzhou"},{"mkOutAmount":100,"mkOutAddr":"shanghai"},{"mkOutAmount":100,"mkOutAddr":"Beijing"}],"mkInputs":[]}]},{"mkHeader":{"mkNonce":748,"tag":"ChainedBlockHeader","mkPrevHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelRoot":894334348103117156},"mkMerkelTree":{"tag":"MerkelLeaf","contents":894334348103117156},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":50,"mkOutAddr":"Guangzhou"},{"mkOutAmount":40,"mkOutAddr":"Shanghai"}],"mkInputs":[{"mkInAddr":"Guangzhou","mkInAmount":100}]}]}]%
```