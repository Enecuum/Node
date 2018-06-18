## Node

P2P node for the main network protocol.

The node acts as a carrier and data storage. Numerous nodes form a network to transmit arbitrtary data across: messages about new k-blocks, microblocks, transactions, events, etc. All the data carried between nodes is ecnrypted (asymmetric encryption).

Compiling the repo generates the following executables:

* BootNode-exe - a boot node that keeps a list of arbitrary available connected Simple Nodes and gives parts of that list on request to new Simple Nodes entering the network.
* SimpleNode-exe - a basic role of a node that receives transactions from LighClient (user light client), adds them to the mempool, sends PoA Nodes (mobile nodes) transactions from the mempool, receives generated microblocks from PoA Nodes and propagates those blocks over the network to other Simple Nodes. Also,it can receive requests to calculate wallet balance, give information on transactions and microblocks, and then give responses to those requests. In the future, it'll be used in the sharding implementation: data distribution management and data fetchon request.
* LightClien-exe - a user client that can generate new public keys (wallet addresses), generate transacations and send them to an arbitrary node (can be a local or remote node).

Node supports multithreading. Each thread has its own data and can read data from another thread, change its (.self) state, propagate messages to other actors and create new actors. This way we avoid numerous problems normally assosiated with multithreading and can process events in parallel balancing the available resources.

Node relies on actors. The central part of a node is the governing actor. It stores the main data, information about neighbor nodes and network status information. Other actors are reponsible for communication with the outside world processing incoming and outcoming messages.


<br/>
<br/>

## RCP-JSON API for Simple Node

**Sends transaction to a node: `enq_sendTransaction`**

##### Request parameters

- `signature` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `amount` - amount intended for transfer
- `owner_key` - sender's public key (a.k.a your address in the network)
- `receiver_key` - receiver's public key (a.k.a their address in the network)

##### Response parameters

 - none if the transaction successfully got to the mempool. In case the node is down there is no response at all.

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{"jsonrpc":"2.0","params":{"x":{"signature":{"sign_s":94223551497283667262425597401930625553013615916376364863948543283948993734318,"sign_r":28834183127673934019323833968330946475134237099239544917311924861094696131869},"amount":10, "time":53045.910023792, "owner_key":117770961709617055389350520036565206405403916684488363141903718453974738717275658,"receiver_key":117770961709617055389350520036565206405403916684488363141903718453974738717275658}}, "method":enq_sendTransaction, "id":1}`

##### Response example:

###### Success:
 
`{"result":[],"jsonrpc":"2.0","id":1}`

###### Error: failed to parse the JSON object:

`{"error":{"code":-32700,"message":"Invalid JSON"},"jsonrpc":"2.0","id":null}`

###### Error: access from a restricted IP address

`{"error":{"code":401,"message":"Access denied: wrong IP"},"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get information about transaction: `enq_getTransactionByHash`**

##### Request parameters

- `hash` - hash of the target transaction

##### Response parameters

- `signature` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `amount` - amount intended for transfer
- `owner_key` - sender's public key (a.k.a your address in the network)
- `receiver_key` - receiver's public key (a.k.a their address in the network)
- `block` - hash of the target (micro)block
- `index` - index of the transaction in the target (micro)block

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Response example:

`"jsonrpc":"2.0","params":{"hash":"6668736C666B68"},"method":"enq_getTransactionByHash","id":1}`

##### Response example:

`{"result":{"tx":{"signature":{"sign_s":9801959209846992852311330047916112131397328350695846174818753987431086006404,"sign_r":51905750445091806981764453120165643019701345224570532370988964470189867234685},"amount":10,"time":58944.703034014,"owner_key":98123844696921525574085363646516869903415165222879705991840115754920922153509,"receiver_key":7390378781922706300737074641115163931396417816966376357730577494300556106681},"block":"7364666C6A736B6668676C6B6A6864","index":10},"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get information about block: `enq_getBlockByHash`**

##### Request parameters

- `hash` - hash of the target block

##### Response parameters

- `signature` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `amount` - amount intended for transfer 
- `owner_key` - sender's public key (a.k.a your address in the network)
- `receiver_key` - receiver's public key (a.k.a their address in the network)
- `hashPreviousMicroblock` - hash of the previous (micro)block
- `hashCurrentMicroblock` - hash of the target (micro)block


###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version


##### Request example:

`{"jsonrpc":"2.0","params":{"hash":"68646A66686C666A6B686C"},"method":"enq_getBlockByHash","id":1}`

##### Response example:

`{"result":{"trans":[{"signature":{"sign_s":9801959209846992852311330047916112131397328350695846174818753987431086006404,"sign_r":51905750445091806981764453120165643019701345224570532370988964470189867234685},"amount":10,"time":58944.703034014,"owner_key":98123844696921525574085363646516869903415165222879705991840115754920922153509,"receiver_key":7390378781922706300737074641115163931396417816966376357730577494300556106681}],"hashPreviousMicroblock":"64666A736B676A6C6A73","hashCurrentMicroblock":"6664736C6B6A68666C6B"},"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get wallet balance: `enq_getBalance`**

##### Request parameters

- `address` - target address (a.k.a. public key)

##### Response parameters

- `result` contains the wallet balance (int64). For the testnet version, this int64 evaluates to ENQ, later smaller thing is going to be presented (e.g. ENQBit).

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

 
##### Request example: 

`{"jsonrpc":"2.0","params":{"address":"B026nJ7HB2nPtPjxkTrH2V5PzhgsWWab1gpi29oLsfiTKv"},"method":"enq_getBalance","id":1}`

##### Response example:

`{"result":0,"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get transaction history for a wallet: `enq_getAllTransactions`**

##### Request parameters

- `address` - target address (a.k.a. public key)

##### Response parameters

- `signature` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `amount` - amount intended for transfer 
- `owner_key` - sender's public key (a.k.a your address in the network)
- `receiver_key` - receiver's public key (a.k.a their address in the network)

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{"jsonrpc":"2.0","params":{"address":"B026nJ7HB2nPtPjxkTrH2V5PzhgsWWab1gpi29oLsfiTKv"},"method":"enq_getAllTransactions","id":1}`

##### Response example:

`{"result":[{"signature":{"sign_s":9801959209846992852311330047916112131397328350695846174818753987431086006404,"sign_r":51905750445091806981764453120165643019701345224570532370988964470189867234685},"amount":10,"time":58944.703034014,"owner_key":98123844696921525574085363646516869903415165222879705991840115754920922153509,"receiver_key":7390378781922706300737074641115163931396417816966376357730577494300556106681},{"signature":{"sign_s":9801959209846992852311330047916112131397328350695846174818753987431086006404,"sign_r":51905750445091806981764453120165643019701345224570532370988964470189867234685},"amount":10,"time":58944.703034014,"owner_key":98123844696921525574085363646516869903415165222879705991840115754920922153509,"receiver_key":7390378781922706300737074641115163931396417816966376357730577494300556106681}],"jsonrpc":"2.0","id":1}`

<br/>
<br/>

## Build and Install

### Install Haskell Stack

1. Install Haskell stack

`curl -sSL https://get.haskellstack.org/ | sh`

2. If needed, add the path to your profile

`sudo nano ~/.profile` and append `export PATH=$PATH:$HOME/.local/bin` at the end.


### Install Docker and Pull the Image

1. [Install Docker](https://docs.docker.com/install/linux/docker-ce/ubuntu/)

2. Add current user to the docker group

`sudo usermod -a -G docker $USER`

2.1 Log out or reboot

3. Pull Docker image (warning, ~2.6 Gb)

`stack docker pull`


### Clone and Build Node

1. Clone the repo

`git clone git@github.com:Enecuum/Node.git`

2. Build without docker

`stack build`


### Initialize a Boot Node

`stack exec BootNode-exe`


### Initialize a Simple Node

`stack exec SimpleNode-exe`


### Initialize Light Client

`stack exec LightClient-exe`.

#### Available commands for Light Client

| Command shortcut | Full command | Description |
|---------|--------|---------|
| -V, -? | --version | Show version number |
| -K | --get-public-key | Create new public key |
| -G qTx | --generate-n-transactions=qTx | Generate N transactions |
| -F | --generate-transactions | Generate transactions forever |
| -M | --show-my-keys | Show my public keys |
| -B publicKey | --get-balance=publicKey | Get balance for a public key |
| -S amount:to:from:currency | --send-money-to-from=amount:to:from:currency | Send currency from a public key to a public key (ENQ/ETH/DASH/BTC) |

<br/>
<br/>

### (Optional) Set your own environment variables:

You can define curstom values for variables in the /configs/config.json:

* `simpleNodeBuildConfig` is the config section for Simple Node.

| Variable | Description |
|---------|---------|
| rpcPort | Port for remote procedure calls to the node |

<br/>

* `statsdBuildConfig` is the config section for the server to collect metrics. 

| Variable | Description |
|---------|---------|
| host | IP of the stat server; you can write yours if you have one up and running |
| port | Port of the server to send the data to |

<br/>

* `bootNodeList` is the variable for the Boot Node to address when you enter the network the first time or after a while. `1` stands for the ID of the Boot Node, `172,17,0,2` - for the IP address Boot Node resides at (Docker's default IP address in this case). `1667` - port Boot Node receives requests to.

<br/>

* `poaPort` is the variable for communication with PoA Nodes (receiving requests for transactions, sending transactions, receiving microblocks. 

<br/>

* `logsBuildConfig` is the config section for the log server.

| Variable | Description |
|---------|---------|
| host | IP of the log server; you can write yours if you have one up and running |
| port | Port of the server to send the data to |

<br/>

* `extConnectPort` is the variable that defines the port for commnunicating with the outer world (e.g. broadcasting). 

<br/>
<br/>

## Use Cases

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

<br/>
<br/>

## Testing

Launch a Boot Node or Simple Node.\
Execute `stack test`

### UML

```yuml:class
[code]-[note:code2]

```
