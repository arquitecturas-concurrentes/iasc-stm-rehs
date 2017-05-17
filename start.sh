#!/bin/bash

cabal configure && cabal install && /home/franco/.cabal/bin/ping-pong-hs
