## Node

P2P node for the main network protocol.

## Build and Install

### Install Haskell Stack

1. Install Haskell stack

`curl -sSL https://get.haskellstack.org/ | sh`

2. If needed, add the path to your profile

`sudo nano ~/.profile` and append `export PATH=$PATH:$HOME/.local/bin` at the end.

### Install RockDB for the Legder

`sudo apt install librocksdb-dev`

### Install libs for the client

`sudo apt install libtinfo-dev`

### Clone and Build Node

1. Choose the appropriate local folder, clone the repo and change to the cloned repository folder

`git clone https://github.com/Enecuum/Node.git && cd Node`

2. Build & install

`stack install`

### Initialize

`enq-node-haskell initialize ./configs/Client.json`
