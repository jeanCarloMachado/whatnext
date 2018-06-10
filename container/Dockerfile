FROM debian:jessie

RUN apt -y update && \
    apt -y install python3 python3-pip software-properties-common wget

RUN wget https://haskell.org/platform/download/8.4.2/haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz && \
    tar xf haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz && \
    ./install-haskell-platform.sh && \
    rm -rf haskell-platform-8.4.2-unknown-posix--core-x86_64.tar.gz && \
    rm install-haskell-platform.sh && \
    rm hp-usr-local.tar.gz

RUN apt install -y libgmp-dev

RUN cabal update && \
    cabal install aeson

WORKDIR /

