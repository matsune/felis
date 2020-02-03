from ubuntu:16.04

RUN set -x && \
  echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-9 main" >> /etc/apt/sources.list && \
  echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-9 main" >> /etc/apt/sources.list && \
  apt-get update --allow-unauthenticated && \
  apt-get install -y --allow-unauthenticated libllvm-9-ocaml-dev libllvm9 llvm-9 llvm-9-dev llvm-9-doc llvm-9-examples llvm-9-runtime && \
  apt-get install -y --allow-unauthenticated clang-9 clang-tools-9 clang-9-doc libclang-common-9-dev libclang-9-dev libclang1-9 clang-format-9 python-clang-9 clangd-9 && \
  apt-get install -y make cmake gcc g++ python libtool zlib1g zlib1g-dev subversion
 
ENV PATH $PATH:/usr/lib/llvm-9/bin
WORKDIR /felis
