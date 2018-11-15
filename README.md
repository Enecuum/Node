## Node

[![buddy pipeline](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19/badge.svg?token=c35be458f2d393a30001acf59f086401a00713eb057ab070050e9855280788bf "buddy pipeline")](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19)

Node is the project that allows to build network actors and blockchain protocols. It contains:

  - Enecuum.Framework;
  - main enecuum blockchain protocol and nodes;
  - sample nodes;
  - testing environment for nodes.

The goal of the Enecuum.Framework is to make writing of blockchain algorithms and behavior simple.
The framework provides such possibilities:

  - TCP, UDP, JSON-RPC for client and server side;
  - parallel network requests processing;
  - safe and robust concurrent state;
  - parallel computations;
  - concurrent in-memory data graph of arbitrary structure;
  - KV-database support;
  - embeddable console client;
  - arbitrary configs for nodes;
  - basic cryptography;
  - logging;
  - and other features.

## Build and Install

### Install Haskell Stack

1. Install Haskell stack

`curl -sSL https://get.haskellstack.org/ | sh`

2. If needed, add the path to your profile

`sudo nano ~/.profile` and append `export PATH=$PATH:$HOME/.local/bin` at the end.

### Install RocksDB

`sudo apt install librocksdb-dev`

### Install libs for the client

`sudo apt install libtinfo-dev`

### Clone and Build Node

1. Choose the appropriate local folder, clone the repo and change to the cloned repository folder

`git clone https://github.com/Enecuum/Node.git && cd Node`

2. Build & install

`stack build --fast`

3. Run tests (optional) 

Run all tests:
`stack build --fast --test`

Run fast tests:
`stack build --fast --test --test-arguments "-m Fast"`

Run slow and unreliable tests:
`stack build --fast --test --test-arguments "-m Slow"`

### Node executable

`enq-node-haskell` is a single executable for nodes.
`./configs` contains several configs for different nodes.

# Running sample nodes

* GraphNode Transmitter
`stack exec enq-node-haskell initialize ./configs/GraphNodeTransmitter.json`

* GraphNode Receiver
`stack exec enq-node-haskell initialize ./configs/GraphNodeReceiver.json`

* Fake PoW
`stack exec enq-node-haskell initialize ./configs/pow.json`

* Fake PoA
`stack exec enq-node-haskell initialize ./configs/poa.json`

* Console client
`stack exec enq-node-haskell initialize ./configs/Client.json`
