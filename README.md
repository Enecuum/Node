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

Execute `stack docker pull` to download the Docker image.

Clone this repository.

#### Set permissions for the user, and login as root if needed
`sudo usermod -a -G docker $USER`\
`su $USER`

#### Set your own environment variables:
Create a custom config file or use the /configs/config.ini. Define values for the variables:

* `[MakeConfigBootNode]` is the initial config section for a boot node, and `port=1666` is an example port used for communication with the node.

* `[MakeConfigSimpleNode]` is the initial config section for a simple node, and `port=1667` is an example port used for communication with the node.

* `[SimpleNode]` is the main config section for a simple node, and `poa_in=1554` `poa_out=1556` `rpc=1555` are example ports used for communication with the node.

| Variable | Description |
|---------|---------|
| poa_in | Port for incoming requests to generate transactions and receiving microblocks |
| poa_out | Port for for sending transactions and propagating microblocks to the network  |
| rpc | Port for remote procedure calls |

* `[Common]` is the config section for common variables, and `bootNodeList=[(1, (127,0,0,1), 1666)]` are examples of node ID, address and port set as environment variables before running a simple node.

* `[Statsd]` is the config section for the statistical service, and addr `addr=127.0.0.1` and `port=8125` are the address of the service and port to send statistical data to.

* `[Docker]` is the config section for the name of the node, and `name=node` is an example name of the Docker container to build.

#### Build container with a node

Execute `./build.sh` to build stack and a container with a node in it. 

#### Run the container locally

Execute `docker run -it node` where 'node' stands for the name of the Docker image to run.

#### Initialization of a boot node
Execute the following commands to start a node:\
`stack exec MakeConfigBootNode-exe`\
`stack exec BootNode-exe`

You can also use `stack exec MakeConfigBootNode-exe -- configs/config.ini` where `configs/nameOfYourConfig.ini` stands for the path to a custom configuration file. Otherwise, the default `configs/config.ini` is used.

#### Initialization of a simple node

Execute the following commands:\
`stack exec MakeConfigSimpleNode-exe`\
`stack exec SimpleNode-exe`

You can also use `stack exec MakeConfigSimpleNode-exe -- configs/config.ini` where `configs/nameOfYourConfig.ini` stands for the path to a custom configuration file. Otherwise, the default `configs/config.ini` is used. 

#### Initialization of a light client

Execute `stack exec LightClient-exe`.

#### Available commands for a light client

| Command shortcut | Full command | Description |
|---------|--------|---------|
| -V, -? | --version | Show version number |
| -K | --get-public-key | Create new public key |
| -G qTx | --generate-n-transactions=qTx | Generate N transactions |
| -F | --generate-transactions | Generate transactions forever |
| -M | --show-my-keys | Show my public keys |
| -B publicKey | --get-balance=publicKey | Get balance for a public key |
| -S amount:to:from:currency | --send-money-to-from=amount:to:from:currency | Send currency from a public key to a public key (ENQ/ETH/DASH/BTC) |


### Without Docker (Boot and Simple Nodes)

#### Preliminary steps
Install Haskel stack\
`curl -sSL https://get.haskellstack.org/ | sh`

Clone this repository and execute `stack build`.

#### Initialization of a boot node
Execute the following commands to start a node:\
`stack exec MakeConfigBootNode-exe`\
`stack exec BootNode-exe`

You can also use `stack exec MakeConfigBootNode-exe -- configs/config.ini` where `configs/nameOfYourConfig.ini` stands for the path to a custom configuration file. Otherwise, the default `configs/config.ini` is used.

#### Initialization of a simple node

Execute the following commands:\
`stack exec MakeConfigSimpleNode-exe`\
`stack exec SimpleNode-exe`

You can also use `stack exec MakeConfigSimpleNode-exe -- configs/config.ini` where `configs/nameOfYourConfig.ini` stands for the path to a custom configuration file. Otherwise, the default `configs/config.ini` is used. 

#### Initialization of a light client

Execute `stack exec LightClient-exe`.

#### Available commands for a light client

| Command shortcut | Full command | Description |
|---------|--------|---------|
| -V, -? | --version | Show version number |
| -K | --get-public-key | Create new public key |
| -G qTx | --generate-n-transactions=qTx | Generate N transactions |
| -F | --generate-transactions | Generate transactions forever |
| -M | --show-my-keys | Show my public keys |
| -B publicKey | --get-balance=publicKey | Get balance for a public key |
| -S amount:to:from:currency | --send-money-to-from=amount:to:from:currency | Send currency from a public key to a public key (ENQ/ETH/DASH/BTC) |


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
