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


## Build and Install

### Install Haskell Stack

1. Install Haskell stack

`curl -sSL https://get.haskellstack.org/ | sh`

2. If needed, add the path to your profile

`sudo nano ~/.profile` and append `export PATH=$PATH:$HOME/.local/bin` at the end.

### Install RockDB for the Legder

`sudo apt install librocksdb-dev`

### Clone and Build Node

1. Choose the appropriate local folder, clone the repo and change to the cloned repository folder

`git clone https://github.com/Enecuum/Node.git && cd Node`

2. Build

`stack build`

### Initialize a Boot Node

`stack exec BootNode-exe`


### Initialize a Simple Node

`stack exec SimpleNode-exe`


### Initialize Light Client

`stack exec LightClient-exe`

<br>

Hint: to run an instance of LightClient, you need to first start a local Simple Node in the background.\
Alternatively, you can run LightClient with additional parameters `addr` and `port` to connect to a remote Simple Node, e.g.: `stack exec LightClient-exe-- --addr=245.217.53.5 --port=1555`
<br>

#### Available commands for Light Client

| Command shortcut | Full command | Description of the command and parameters |
|---------|--------|---------|
| -P port | --port=port | port number |
| -A hostAddr | --addr=hostAddr |  host address |
| -F walletsFile | --wallets=walletsFile | csv file containing wallets |
| -S transactionsFile | --transactions=transactionsFile | csv file containing transactions to be sent |
|  -K keysCount       |  --gen-keys=keysCount           |   generate N key pairs |
|  -B publicKey       |  --get-balance=publicKey        |  get balance for public key |
|  -M wallets         |  --show-my-keys=wallets         | show my public keys |
|  -U hash            |  --load-block=hash              |   get keyblock by hash |
|  -O hash            |  --load-microblock=hash         |   get microblock by hash |
|  -X hash            |  --get-tx=hash                  |   get transaction by hash |
|  -W publicKey       |  --load-wallet=publicKey        |   get all transactions for a wallet |
|  -I                 |  --chain-info                   |   get total chain info |
|  -R message         |  --send-message-for-all=message |   send broadcast message |
|  -T nodeId message  |  --send-message-to=nodeId message |  send message to the node |
|  -L                 |  --load-new-messages            |   load new recieved messages |
|  -V                 |  --version                      |   show LightClient version |
|  -H, -?             |  --help                         |   help |


<br/>
<br/>

### (Optional) Set your own environment variables:

You can define curstom values for variables in the /configs/config.json:

* `simpleNodeBuildConfig` is the config section for Simple Node.

| Variable | Description |
|---------|---------|
| enableIP | IP addresses that are allowed to connect to the node |
| rpcPort | Port for remote procedure calls to the node |

Hint: setting `enableIP` to `0.0.0.0` or leaving it blank allows any IP addresses. This field allows IPV6 subnet masks as well as IP addresses separated by commas.
 
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


## JSON-RPC API for Simple Node

**Sends transaction to a node: `enq_sendTransaction`**

##### Request parameters

- `amount` - amount intended for transfer
- `uuid` - unique identifier for the transaction
- `owner` - sender's public key (a.k.a your address in the network)
- `receiver` - receiver's public key (a.k.a their address in the network)
- `currency` - currency itended for transfer
- `sign` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `timestamp` - current time (will be deprecated soon)

##### Response parameters

 - none if the transaction successfully got to the mempool. In case the node is down there is no response at all.

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{
  "jsonrpc":"2.0",
  "params":
   {"tx":{"amount":10,"uuid":11,"owner":"4mGYpcwycV9aWBurroUo2UMvCx5NL5RrGbdKacMoPZuX","receiver":"5YXZXhM9MMafhS5fdzanWbimKEpAnmrqabD5BcMm8TxS","currency":"ENQ","sign":    {"sign_s":53851544468490237432689794264655477248827276839794535462123667385563675641643,"sign_r":32540351665388188734732260127968413635181826340120448422375467318877505024005},"timestamp":1529677285}},"method":"enq_sendTransaction","id":1}`

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

- `amount` - amount intended for transfer
- `uuid` - unique identifier for the transaction
- `owner` - sender's public key (a.k.a your address in the network)
- `receiver` - receiver's public key (a.k.a their address in the network)
- `currency` - currency itended for transfer
- `sign` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `timestamp` - current time (will be deprecated soon)
- `block` - has of the microblock this transaction is published (mined) in

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{"jsonrpc":"2.0","params":{"hash":"JM47wo87CqMVtVvit1gWfYdcdGrJM6kRmdMiLh8Jdi7AQdqWvXnrcLXskPFx"},"method":"enq_getTransactionByHash","id":1}`

##### Response example:

`{"result":{"tx":{"amount":15,"uuid":12,"owner":"UGkX5YaDQ2p9tJE4FWK81QmTg3bDsgrW6NWdRbZg1Rzh","receiver":"JW7ecwuBk3mBUsYFbrNzdWFtzg2gZjQfovUhfzFrT9NA","currency":"ENQ","sign":{"sign_s":108896698171168516308183867426676890214358078525015061253627796254851838899532,"sign_r":96833102483162253818415606073695362365268968710929065084732226015632032994116},"timestamp":1529681873},"block":"S005QWNzTmNrM1M4NHlDRllEU2hOZ3dwaVloYVpZMTJzMVp2dkxndlVxbmM=","index":5},"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get information about macroblock: `enq_getBlockByHash`**

##### Request parameters

- `hash` - hash of the target block

##### Response parameters

- `height` - number of blocks from the genesis to the current one 
- `solver` - public key of the PoW solver who opened this macroblock (mined the k-block)
- `txs_cn` - number of transactions in the macroblock
- `prev_hash` - hash of the previous macroblock
- `reward` - reward to the PoW miner for opening the macroblock (mining the k-block)
- `microblocks` - hashes of microblocks inside the macroblock
- `difficulty` - current difficulty level for opening a macroblock (mining a k-block)
- `microblocks_cnt` - number of microblocks inside the macroblock

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{"jsonrpc":"2.0","params":{"hash":"JM47wo87CqMVtVvit1gWfYdcdGrJM6kRmdMiLh8Jdi7AQdqWvXnrcLXskPFx"},"method":"enq_getBlockByHash","id":1}`

##### Response example:

`{"result":{"height":2,"solver":"RV36deizV55S3MnHTUzg3jEirHSDsDEMR89fNAzMadNZ4Fm4dG9PeQm2TAQA","txs_cnt":512,"prev_hash":"W4g6hWkxQhUQrXWmHoLkQocrLzrSgRZvjkm4ASsq4jt3Vqj5aSt3j8jiiTfV","reward":100,"microblocks":["RV36deizV55S3MnHTUzg3jEirHSDsDEMR89fNAzMadNZ4Fm4dG9PeQm2TAQA","JM47wo87CqMVtVvit1gWfYdcdGrJM6kRmdMiLh8Jdi7AQdqWvXnrcLXskPFx","J3fXPmZfaEzu9dp3kJsf8RuQzzRuzEBtCu8ciBiN6mmNkNj6HqSobgwtJ4W3","W4g6hWkxQhUQrXWmHoLkQocrLzrSgRZvjkm4ASsq4jt3Vqj5aSt3j8jiiTfV"],"difficulty":1,"microblocks_cnt":4},"jsonrpc":"2.0","id":1}`

<br>
<br>


**Get information about microblock: `enq_getMicroblockByHash`**

##### Request parameters

- `hash` - hash of the target block

##### Response parameters

- `transactions` - start of an array of transactions in the microblock
- `amount` - amount intended for transfer
- `uuid` - unique identifier for the transaction
- `owner` - sender's public key (a.k.a your address in the network)
- `receiver` - receiver's public key (a.k.a their address in the network)
- `currency` - currency itended for transfer
- `sign` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `timestamp` - current time (will be deprecated soon)
- `k_block` - hash of the k-block (k-block is the opening part of a macroblock) this microblock is linked to
- `txs_cnt` - number of transactions in the microblock
- `reward` - reward for publishing a microblock
- `publishers` - public keys (addresses) of PoA Nodes in the PoA Team (used for indexing microblocks in the macroblock)
- `sign` - ECDSA signature of the PoA Node that published the microblock
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `index` - index of the microblock in the macroblock 


###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version


##### Request example:

`{"jsonrpc":"2.0","params":{"hash":"JM47wo87CqMVtVvit1gWfYdcdGrJM6kRmdMiLh8Jdi7AQdqWvXnrcLXskPFx"},"method":"enq_getMicroblockByHash","id":1}`

##### Response example:

`{"result":{"transactions":[{"amount":10,"uuid":15,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":111453389016965411942432161422836677977682117261774497318558333020708965069823,"sign_r":34802528517330025474056637040554169615944272750680678430210531325764022406021},"timestamp":1529681025},{"amount":19,"uuid":12,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":36702896446615718903997830217307609909443281366110703304854624965337681147887,"sign_r":7367170092931138165337844383547165997062478109283580450989454932653837213781},"timestamp":1529681025},{"amount":14,"uuid":19,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":98414524750868806711785718518430616768375010094240450345905375658581001458859,"sign_r":95027401014124111353895671545669534279352130813036148099458965611942353578063},"timestamp":1529681025}],"k_block":"W4g6hWkxQhUQrXWmHoLkQocrLzrSgRZvjkm4ASsq4jt3Vqj5aSt3j8jiiTfV","txs_cnt":3,"reward":1,"publishers":[],"sign":{"sign_s":111453389016965411942432161422836677977682117261774497318558333020708965069823,"sign_r":34802528517330025474056637040554169615944272750680678430210531325764022406021},"index":4},"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get stats on the global state: `enq_getChainInfo`**

##### Request parameters

- none specific parameters

##### Response parameters

- `nodes_num` - total number of mining nodes
- `blocks_num` - total number of mined macroblocks
- `difficulty` - current difficulty level of opening a macroblock (mining k-block)
- `emission` - total emission 

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version


##### Request example:

`{"jsonrpc":"2.0","method":"enq_getChainInfo","id":1}`

##### Response example:

`{"result":{"nodes_num":4,"blocks_num":10,"difficulty":1,"txs_num":512,"emission":10000},"jsonrpc":"2.0","id":1}`

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

`{"jsonrpc":"2.0","params":{"address":"4mGYpcwycV9aWBurroUo2UMvCx5NL5RrGbdKacMoPZuX"},"method":"enq_getBalance","id":1}`

##### Response example:

`{"result":0,"jsonrpc":"2.0","id":1}`

<br>
<br>

**Get transaction history for a wallet: `enq_getAllTransactions`**

##### Request parameters

- `address` - target address (a.k.a. public key)

##### Response parameters

- `amount` - amount intended for transfer
- `uuid` - unique identifier for the transaction
- `owner` - sender's public key (a.k.a your address in the network)
- `receiver` - receiver's public key (a.k.a their address in the network)
- `currency` - currency itended for transfer
- `sign` - ECDSA signature 
- `sign_s` - represents [R output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature  
- `sign_r` - represents [S output](https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm#Signature_generation_algorithm) in the signature 
- `timestamp` - current time (will be deprecated soon)

###### Misc:

- `method` - target API method
- `id` - auto-assigned JSON-RPC ID (used to match the response object with the request object)
- `jsonrpc` - JSON-RCP version

##### Request example:

`{"jsonrpc":"2.0","params":{"address":"5YXZXhM9MMafhS5fdzanWbimKEpAnmrqabD5BcMm8TxS"},"method":"enq_getAllTransactions","id":1}`

##### Response example:

 `{"result":[{"amount":10,"uuid":15,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":111453389016965411942432161422836677977682117261774497318558333020708965069823,"sign_r":34802528517330025474056637040554169615944272750680678430210531325764022406021},"timestamp":1529681025},{"amount":19,"uuid":12,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":36702896446615718903997830217307609909443281366110703304854624965337681147887,"sign_r":7367170092931138165337844383547165997062478109283580450989454932653837213781},"timestamp":1529681025},{"amount":14,"uuid":19,"owner":"K6Cw7fqXz1VD2ZmZvYvZd395NR8wNngE6Q1EgUHvw5Us","receiver":"CmCy17EfeRdVLp8FdgT5BJjbHH2HHPcZ83BiCsMMqbNL","currency":"ENQ","sign":{"sign_s":98414524750868806711785718518430616768375010094240450345905375658581001458859,"sign_r":95027401014124111353895671545669534279352130813036148099458965611942353578063},"timestamp":1529681025}],"jsonrpc":"2.0","id":1}`

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
