# simhash

htm.core SimHash Runner

# Build

Recommend build `simhash` with [`stack`](https://docs.haskellstack.org/en/stable/README/)

    git clone https://github.com/Lupino/simhash.git
    cd simhash
    git submodule update --init
    mkdir bin
    echo 'local-bin-path: bin' >> stack.yaml
    stack build
    stack install