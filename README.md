## Node

P2P Node for main  network protocol.

The node acts as a carrier and data storage. Numerous nodes form a network to transmit arbitrtary data across: messages about new blocks, transactions, events, serialized specific messages, etc. All the data carried between nodes is ecnrypted (public-private key pair).

There are two types of nodes:

* Boot nodes. They keep a list of arbitrary available connected nodes and give parts of that list on request to simple nodes connecting to the network.
* Simple (basic) nodes receive transactions from clients and add them to the pending pool, give PoA nodes transactions from the pending pool, receive blocks from PoA nodes and propagate those blocks to other simple nodes, and calculate and send clients their balances. In the future it'll be used in the sharding implementation: data distribution management and fetching data on request.

Node supports multithreading. Each thread has its own data and can read data from another thread, change its (self) state, propagate messages to other actors and create new actors. This way we avoid numerous problems normally assosiated with multithreading and can process events in parallel balancing the available resources.

Node relies on actors. The central part of a node is the governing actor. It stores the main data, information about neighboring nodes and network status information. Other actors are reponsible for communication with the outside world processing incoming and outcoming messages.

LightClient is a client allowing to connect to a node (local or remote), send transactions and calculate balance via the ledger.

## How to Use

### In a Docker Container (Simple Node)

#### Prelimiary steps
[Install Docker](https://docs.docker.com/install/linux/docker-ce/ubuntu/)

Clone this repository and execute `stack build`.

#### Set permissions for the user, and login as root if needed
`sudo usermod -a -G docker $USER`\
`su $USER`

#### Build container with a node

Execute `stack image container` to build a container with a node in it. 

#### Run the container locally

Execute `docker run -it node` where 'node' stands for the pre-defined (in the stack.yaml) name of the node.


### Without Docker (Boot and Simple Nodes)

#### Preliminary steps
Install Haskel stack\
`curl -sSL https://get.haskellstack.org/ | sh`

Clone this repository and execute `stack build`.

#### Configuration
First time you start a node (boot or simple), it needs initial configuration.
Starting should begin from boot nodes. Then simple nodes.

#### Initialization of a boot node
Execute the following commands to start a node:\
`stack exec MakeConfigBootNode-exe`\
`stack exec BootNode-exe`

#### Initialization of a simple node

Set your own environment variables for a simple node:
* statsd defines the address (format is 0.0.0.0) to send statistical data to the defined port (format is 0000).
* bootNodeList defines the address and port list of the boot nodes that can be asked for addresses of other nodes to connect to. Use the format [(NodeId, IP, port)] where IP is in the format [(0.0.0.0)] and the port is your chosen port.
* export statsd - export statistical data to the statistical service to the defined IP address (format is 0.0.0.0)
* export bootNodeList - use the format [(1,(2,3,4,5), 0000)] where 1 stands for the required nodeID, (2,3,4,5) stands for the IP address of the boot node, and 0000 stands for the port.

Execute the following commands:\
`export bootNodeList="[(1, (2,3,4,5), 0000)]"`\
`export statsd="0.0.0.0"`\
`stack exec MakeConfigSimpleNode-exe`\
`stack exec SimpleNode-exe`

#### Initialization of a light client

After you've started a simple node, execute `stack exec LightClient-exe`.

#### Available commands for a light client

| Command shortcut | Full command | Description |
|---------|--------|---------|
| -V, -? | --version | Show version number |
| -K | --get-public-key | Create new public key |
| -G qTx | --generate-n-transactions=qTx | Generate N transactions |
| -F | --generate-transactions | Generate transactions forever |
| -M | --show-my-keys | Show my public keys |
| -B publicKey | --get-balance=publicKey | Get balance for public key |
| -S amount:to:from:currency | --send-money-to-from=amount:to:from:currency | send money to wallet from wallet (ENQ | ETH | DASH | BTC) |



### Use Cases

#### Creating the initial identifying public key.

To start operating as a simple node in the network, you'd run the first four initialiation commands (cf. "Initialization of a simple node") then you'd run `-K` to create your initial public key identifying you in the network. To this public key you can receive transactions as well as generate and send them from this public key.


#### Creating a second public key.

You can create a large number of public keys for a single node. Let's say you want to generate and use a second public key as the address to (one of) your wallet(s). In this case , you'd run the initial four commands to initialize a node (cf. "Initialization of a simple node"), then run `-K ` to create your initial public key and then run `-K` again to create a second public key intended to be used as your wallet address (for this node). Then you'd be able to run send and receive payments form and to through the second public address using it a wallet.


#### Checking your public keys

In the case you want to check the public keys you generated overtime, you'd run `--show-my-keys`.


#### Generating a transaction and sending it to another participant (public key)

After you've created enough public keys, you may want to act as an active member of the network and, say, send transactions.
To send a transaction, you'd need to run `-S amount:to:from:currency` where `amount` stands for the amount intended to be send, `to` stands for the public key of the receiver, `from` stands for your chosen public key, and `currency` stands for the currency intended to be used.\
Your transaction still needs to be validated before it reaches the receives, so it is automatically propagated to the pending pool. Then you'd wait till your transaction is validated, i.e. included in a block.


#### Generating a test transaction or unlimited number of test transactions

In the case you want to test this functionality, you'd run `-G qTx` where `qTx` stands for the chosen number of transactions or `-F` for an unlimited number of transactions and check the results.


#### Checking the balance of a public key

In the case you want to know the balance of a public key, you'd run `-B publicKey` where `publicKey` stands for the chosen public key.


### Testing

Launch the node.\
Execute `stack test`
