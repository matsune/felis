#!/bin/sh
BIN="../bin/Debug/felisc"
$BIN ../src/internal/felis_lib.felis
$BIN $1.felis
gcc felis_lib.o $1.o
