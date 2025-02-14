#!/usr/bin/env sh

# Be sure to run this in the Nix shell!

tigerbeetle format \
            --cluster=0 \
            --replica=0 \
            --replica-count=1 \
            --development \
            0_0.tigerbeetle
