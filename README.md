# felis
Felis Programming Language

## Build (docker)
```shell
# build image
$ docker build -t felis .

# run image
$ docker run -it --rm -v $(pwd):/felis felis
/felis# mkdir -p build
/felis# cd build
/felis# cmake ..
/felis# make
/felis# ls ../bin
```
