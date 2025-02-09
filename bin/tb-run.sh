#!/usr/bin/env sh

# Make sure you run this from the Nix shell!

# Run a local Tigerbeetle server in the background
# Output stdout and stderr to 0_0.log

tigerbeetle start \
            --addresses=3000 \
            --development \
            0_0.tigerbeetle \
            > 0_0.log 2>&1 &
