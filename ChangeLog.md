# Node project changelog

## Changes in 0.5.0
  - New architecture, design and approaches to build network nodes and blockchain protocols.
    The architecture is now based on Free monads, STM and eDSLs.
    Nodes are the interpretable monadic scripts.
  - Enecuum.Framework (library). Features:
      - TCP, UDP, JSON-RPC for client and server side
      - Parallel network requests processing
      - Arbitrary concurrent state handling
      - Parallel computations and in-app processes
      - Concurrent in-memory data graph of arbitrary structure
      - KV-database support, rocksdb as implementation
      - Embeddable console client
      - Arbitrary configs for nodes
      - Basic cryptography
      - Basic random generation
      - Basic configurable logging (file, console)
  - Sample nodes (executables):
      - GraphNode Transmitter. Works with blockchain graph and ledger.
        Accepts K-blocks and microblocks, has a wide API.
      - GraphNode Receiver. Implements a basic synchronisation scenario.
      - PoW. Fake Proof-of-Work node that generates fake K-blocks (without hash complexity).
      - PoA. Fake Proof-of-Action node that generates random transactions and microblocks.
      - Client. Provides a control over other nodes via RPC API. Handles user commands from stdin.
  - Configs for sample nodes (see ./configs).
  - Integration and acceptance tests
  - Limited functional testing environment
  - Unit and functional tests

# Legacy, old versioning

## Changes in node-haskell-legacy (06b82eb)
  - The latest commit with legacy code.

## Changes in Bambino.v0.1.3 (06b82eb)
  - Special version of Enecuum wallet that works with TestNet Alfa.

## Changes in v2.1 (6f91459)
  - The first attempt to build Enecuum blockchain protocol.
    The code contains several network nodes and some part of Enecuum blockchain
    protocol, but satisfying the requirements was not complete.
