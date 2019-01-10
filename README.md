# Enecuum Node Framework

[![buddy pipeline](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19/badge.svg?token=c35be458f2d393a30001acf59f086401a00713eb057ab070050e9855280788bf "buddy pipeline")](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19)

Enecuum Node Framework allows to build network actors and blockchain protocols, console applications, work with KV database and cryptography. Current features include:

  - Framework to build stateful multithreaded applications
  - Config management tools
  - Testing environment
  - TCP, UDP, JSON-RPC for client and server side
  - Parallel network requests processing
  - Concurrent state based on Software Transactional Memory
  - Parallel computations and processes
  - Concurrent in-memory data graph of arbitrary structure
  - KV-database support
  - Embeddable console client to build interactable CLIs
  - Basic cryptography and random numbers generation
  - Logging
  - Time, file system, and other possibilities

The Node project contains:

  - Enecuum Node Framework
  - Sample nodes with configs
  - Testing environment
  - Tests (functional, integration)

## Framework structure

  - Source code located in [./src/](./src/)
  - Configs for sample nodes located in [./configs/](./configs/)
  - Test code located in [./test/](./test/)

The framework represents a set of embedded monadic languages organized hierarchically. The languages are divided to core languages responsible for common subsystems and framework languages responsible for network and actors behavior.

### Core languages

  * HGraphL - Working with generic any structure graph (concurrently).
  * StateL - Working with concurrent state variables. Represents a wrapper around native STM.
  * DatabaseL - Raw KV database interface. RocksDB is the implementation currently.
  * LoggerL - Logging possibilities.
  * FileSystemL - Working with file system.
  * RandomL - Random generation and crypto methods.
  * CryptoL - Subset to work with crypto methods.
  * TimeL - Getting current time.
  * ControlFlowL - Controlling the flow of the evaluation.

### Framework languages

  * NodeDefinitionL - Language to define servers, APIs for node, command line methods. Provides methods for parallel process forking (forkIO essentially).
  * NodeL - Allows to work with connections (TCP, UDP), create graphs and databases, evaluate scripts in core languages. Also, has methods to evaluate database and state scripts.

## Build, Install, Run

### Install Haskell Stack

1. Install Haskell stack

`curl -sSL https://get.haskellstack.org/ | sh`

2. If needed, add the path to your profile

`sudo nano ~/.profile` and append `export PATH=$PATH:$HOME/.local/bin` at the end.

### Install External dependencies

`sudo apt update && sudo apt install librocksdb-dev libgd-dev libtinfo-dev -y`

### Build Node

1. Clone repo:

`git clone https://github.com/Enecuum/Node.git && cd Node`

2. Build & install

`stack build`

3. Run tests (optional)

Run all tests:

`stack test`

Run fast tests:

`stack build --test --test-arguments "-m Fast"`

Run slow and unreliable tests:

`stack build --test --test-arguments "-m Slow"`

### Sample nodes

`enq-test-node-haskell` is a single executable for sample nodes.

  * GraphNode Transmitter
    - Controllable from the Client node.
    - Accepts K-blocks and microblocks.
    - Works with blockchain graph and ledger.
    - Answers for balance requests.
    - Has a wide API, can answer to many different requests.

    `stack exec enq-test-node-haskell singlenode ./configs/tst_graph_node_transmitter.json`

  * GraphNode Receiver
    - Works with blockchain graph and ledger.
    - Polls the Transmitter node to synchronize with it. Implements a basic synchronisation scenario.

    `stack exec enq-test-node-haskell singlenode ./configs/tst_graph_node_receiver.json`

  * Gen PoW
    - Controllable from the Client node.
    - By the command from Client, generates KBlocks organized in a chain, but without hash complexity (does not do any mining).
    - Sends KBlocks to GraphNode Transmitter.

    `stack exec enq-test-node-haskell singlenode ./configs/tst_gen_pow.json`

  * Gen PoA
     - Polls the Transmitter and generates a microblock for an empty KBlock found.
     - Fills the microblock by random transactions for random wallets (5 wallets are hardcoded).

    `stack exec enq-test-node-haskell singlenode ./configs/tst_gen_poa.json`

  * Console client
    - Has console API.
    - Allows to create wallets, send transactions, ask balance.
    - Sends commands to nodes.

    `stack exec enq-test-node-haskell singlenode ./configs/tst_client.json`

# Node code sample

  * Server logic: [Enecuum.Samples.Assets.Nodes.TstNodes.PingPong.PingServer](./src/Enecuum/Samples/Assets/Nodes/TstNodes/PingPong/PingServer.hs)
  * Client logic: [Enecuum.Samples.Assets.Nodes.TstNodes.PingPong.PongClient](./src/Enecuum/Samples/Assets/Nodes/TstNodes/PingPong/PongClient.hs)
  * Configs:
    - [./configs/tst_ping_server.json](./configs/tst_ping_server.json)
    - [./configs/tst_pong_client1.json](./configs/tst_pong_client1.json)
    - [./configs/tst_pong_client2.json](./configs/tst_pong_client2.json)

In this sample, two nodes interact via network sending UDP messages.
  * Ping server node
    - Listens UDP port for `Ping` messages.
    - Sends `Pons` message back to the client.
    - Manages a concurrent internal state (counter of pings).

    `stack exec enq-test-node-haskell singlenode ./configs/tst_ping_server.json`

  * Pong client node
    - Sends `Ping` messages to the server periodically.
    - Accepts `Pong` messages from the server.

    `stack exec enq-test-node-haskell singlenode ./configs/tst_pong_client1.json`

    `stack exec enq-test-node-haskell singlenode ./configs/tst_pong_client2.json`

### Network messages

```haskell
-- Messages
newtype Ping = Ping Text deriving (Generic, ToJSON, FromJSON)
newtype Pong = Pong Int  deriving (Generic, ToJSON, FromJSON)
```

### Server node

```haskell
 -- Ping server node unique tag.
data PingServerNode = PingServerNode

-- Ping server node config.
data instance NodeConfig PingServerNode = PingServerNodeConfig
    { stopOnPing  :: Int
    , servingPort :: PortNumber
    }

-- Ping server node definition type.
instance Node PingServerNode where
    data NodeScenario PingServerNode = PingServer
    getNodeScript PingServer = pingServerNode
    getNodeTag _ = PingServerNode

-- Handling Ping messages.
acceptPing
    :: D.StateVar D.NodeStatus
    -> D.StateVar Int
    -> Int
    -> Ping
    -> D.Connection D.Udp
    -> L.NodeL ()
acceptPing status pingsCount threshold (Ping clientName) conn = do
    pings <- L.atomically $ do
        L.modifyVar pingsCount (+1)
        L.readVar pingsCount

    let done = pings + 1 >= threshold
    when done $ do
        L.close conn
        L.writeVarIO status D.NodeFinished
        L.logInfo $ "Pings threshold reached: " +|| threshold ||+ ". Finishing."

    unless done $ do
        L.send conn (Pong pings)
        L.logInfo $ "Ping #" +|| pings ||+ " accepted from " +|| clientName ||+ "."

-- Ping server definition node.
pingServerNode :: NodeConfig PingServerNode -> L.NodeDefinitionL ()
pingServerNode cfg = do
    let threshold = _stopOnPing cfg
    let port = _servingPort cfg

    pingsCount <- L.newVarIO 0
    status     <- L.newVarIO D.NodeActing

    -- Starting a separate process for serving on UDP port.
    L.serving D.Udp port $
        L.handler $ acceptPing status pingsCount threshold

    L.awaitNodeFinished' status
```

### Client node

```haskell
-- Pong client node unique tag.
data PongClientNode = PongClientNode
    deriving (Show, Generic)

-- Pong client node config.
data instance NodeConfig PongClientNode = PongClientNodeConfig
    { _clientName        :: Text
    , _pingDelay         :: Int
    , _pingServerAddress :: D.Address
    }
    deriving (Show, Generic)

-- Pong client node definition type.
instance Node PongClientNode where
    data NodeScenario PongClientNode = PongClient
        deriving (Show, Generic)
    getNodeScript _ = pongClientNode'
    getNodeTag _ = PongClientNode

-- Accepting pong responses from the server.
acceptPong :: Pong -> connection -> L.NodeL ()
acceptPong (Pong pingsCount) _ =
    L.logInfo $ "Pong accepted from server. Pings count: " <> show pingsCount

-- Sending pings to the server.
pingSending :: D.StateVar D.NodeStatus -> NodeConfig PongClientNode -> D.Connection D.Udp -> L.NodeL ()
pingSending status cfg conn = do
    L.delay $ _pingDelay cfg
    L.logInfo "Sending Ping to the server."
    eSent <- L.send conn (Ping $ _clientName cfg)
    case eSent of
        Right () -> pingSending status cfg conn
        Left _   -> do
            L.logInfo "Server is gone."
            L.close conn
            L.writeVarIO status D.NodeFinished

-- Pong client definition node.
pongClientNode :: NodeConfig PongClientNode -> L.NodeDefinitionL ()
pongClientNode cfg = do
    status <- L.newVarIO D.NodeActing

    -- Connecting to the server.
    mbConn <- L.open D.Udp (_pingServerAddress cfg) $
        L.handler acceptPong

    case mbConn of
        Nothing -> L.logError "Ping Server not found"
        Just conn -> do
            -- Forking separate process of periodical pings.
            L.process (pingSending status cfg conn)
            -- Waiting when the node is finished.
            L.awaitNodeFinished' status
```

# Additional materials

  * [Building network actors with Enecuum Node Framework (tutorial)](https://gist.github.com/graninas/9beb8df5d88dda5fa21c47ce9bcb0e16)
  * [Why Haskell?](https://medium.com/@ENQBlockchain/why-haskell-eacb087f3adb)
  * [Enecuum. Framework possibilities](https://medium.com/@ENQBlockchain/enecuum-framework-possibilities-d4fa49c3ea40)
  * [Enecuum.Framework Possibilities, Part 2](https://medium.com/@ENQBlockchain/enecuum-framework-possibilities-part-2-7c8ff65c1c4e)
