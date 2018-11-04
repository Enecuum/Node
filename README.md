## Node

[![buddy pipeline](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19/badge.svg?token=c35be458f2d393a30001acf59f086401a00713eb057ab070050e9855280788bf "buddy pipeline")](https://buddy.enecuum.com/enecuum/node/pipelines/pipeline/19)

P2P node for the main network protocol.

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

`stack build --fast --test`

### Initialize

If you added path to your profile:
`enq-node-haskell initialize ./configs/Client.json`

Or you need to use that instead:
`stack exec enq-node-haskell initialize ./configs/Client.json`
