#!/usr/bin/env bash

diff -y <(cabal run day3-recursion-scheme.hs fib1 10 2>&1) <(cabal run day3-recursion-scheme.hs fib2 10 2>&1)
