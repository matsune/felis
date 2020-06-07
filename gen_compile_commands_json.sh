#!/bin/sh
unlink compile_commands.json
cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
ln -s Debug/compile_commands.json .
