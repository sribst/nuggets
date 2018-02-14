#!/bin/bash

CONTAINER="nuggets"

# build container
docker build -t $CONTAINER .
# lunch with interactive mode
docker run --cpuset-cpus="0-3" -it $CONTAINER 
