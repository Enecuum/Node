#!/usr/bin/env bash

variable=$(grep -Pzo "\[Docker\]\n((.*\n)+)(\[.*\])?" config.ini | awk -F= '/name=(.*)/ { print($2); exit; }' -)
if ! [[ $variable ]]; then
	variable="docker-node"
fi
sed  "s/\${node}/$variable/g" stack.yaml.template > stack.yaml
stack build
echo  "Project was built in docker container $variable"
