# bitcoin-h - a simple proof-of-concept haskell/blockchain implementation

This simple project aims to implement the bitcoin system as a proof-of-concept.

It depends heavily on the [Servant](http://hackage.haskell.org/package/servant) library to provide RESTful APIs for node/node, node/server communication

## The Design

TODOs

## Usage

For mocking bitcoin server, run
``` bash
$ cabal new-build
$ cabal new-run
```

For mocking bitcoin nodes, run
``` bash
$ cabal new-repl
*Server> import Node

*Server Node> forkIO $ runNode 19901
*Server Node> forkIO $ runNode 19902
...
```

## APIs

### Server API

```
/nodes
```

## Examples

_Supposing that server is running on port 19900, two nodes on ports 19901/19902_

List all the nodes
``` bash
$ curl --header "Content-Type: application/json" --request POST --data '{"mkInputs":[{"mkInAddr":"Shanghai","mkInAmount":40}],"mkOutputs":[{"mkOutAddr":"Guangzhou","mkOutAmount":10}, {"mkOutAddr":"Shanghai","mkOutAmount":30}]}' http://localhost:19900/newTransaction

"RegisterSuccess"%
```

Show the GenesisBlock

``` bash
$ curl --header "Content-Type: application/json" --request GET http://localhost:19901/blocks

[{"mkHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelTree":{"tag":"MerkelLeaf","contents":7694568972639932276},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":100,"mkOutAddr":"Guangzhou"},{"mkOutAmount":100,"mkOutAddr":"shanghai"},{"mkOutAmount":100,"mkOutAddr":"Beijing"}],"mkInputs":[]}]}]%
```

New Transaction
``` bash
$ curl --header "Content-Type: application/json" --request POST --data '{"mkInputs":[{"mkInAddr":"Guangzhou","mkInAmount":100}],"mkOutputs":[{"mkOutAddr":"Guangzhou","mkOutAmount":50}, {"mkOutAddr":"Shanghai","mkOutAmount":40}]}' http://localhost:19900/newTransaction

"RegisterSuccess"%
```

Show the mined blocks
``` bash
$ curl --header "Content-Type: application/json" --request GET http://localhost:19902/blocks

[{"mkHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelTree":{"tag":"MerkelLeaf","contents":7694568972639932276},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":100,"mkOutAddr":"Guangzhou"},{"mkOutAmount":100,"mkOutAddr":"shanghai"},{"mkOutAmount":100,"mkOutAddr":"Beijing"}],"mkInputs":[]}]},{"mkHeader":{"mkNonce":748,"tag":"ChainedBlockHeader","mkPrevHeader":{"mkNonce":9675,"tag":"ChainedBlockHeader","mkPrevHeader":{"tag":"Genesis"},"mkMerkelRoot":7694568972639932276},"mkMerkelRoot":894334348103117156},"mkMerkelTree":{"tag":"MerkelLeaf","contents":894334348103117156},"mkTransactions":[{"mkOutputs":[{"mkOutAmount":50,"mkOutAddr":"Guangzhou"},{"mkOutAmount":40,"mkOutAddr":"Shanghai"}],"mkInputs":[{"mkInAddr":"Guangzhou","mkInAmount":100}]}]}]%
```