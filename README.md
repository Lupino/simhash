# simhash

htm.core SimHash Runner

# Build

Recommend build `simhash` with [`stack`](https://docs.haskellstack.org/en/stable/README/)

    git clone https://github.com/Lupino/simhash.git
    cd simhash
    git submodule update --init
    stack build
    stack install --local-bin-path bin

# Usage

    $ ./bin/simhash-runner --help
    Usage: simhash-runner [-f|--file FILE] COMMAND [--version]
      SimHash Runner

    Available options:
      -f,--file FILE           SimHash model file
      --version                Print version information
      -h,--help                Show this help text

    Available commands:
      train                    Train simhash model
      test                     Test a string
      infer                    Run infer task
      infer-learn              Run infer learn task
      v2-train                 Train simhash model v2
      v2-test                  Test a string v2
      v2-infer                 Run infer task v2
      v2-infer-learn           Run infer learn task v2


# Train and valid Sample format

    label, string


# V1

## Train

    $ ./bin/simhash-runner -f sample/v1-sample train -d sample/train_data.txt -t sample/valid_data.txt
    Train iters 6/6 100.0%
    Train Spent 0s
    Train Finished in 0s
    Total Spent 0s
    Test iters 6/6 100.0%
    Test score 66.66%
    Test Spent 0s
    Test Finished in 0s
    Total Spent 0s

## Test

    $ ./bin/simhash-runner -f sample/v1-sample test -s 'test data 1'
    [("label1",0.5322839697942492),("label2",0.46771603691875463)]


## Deploy Infer

    $ ./bin/simhash-runner -f sample/v1-sample infer -H tcp://127.0.0.1:5000 -n v1-sample -w 10 -s 5


## Deploy Infer Learn

    $ ./bin/simhash-runner -f sample/v1-sample infer-learn -H tcp://127.0.0.1:5000 -n v1-sample -w 10

# V2

## Train

    $ ./bin/simhash-runner -f sample/v2-sample v2-train -d sample/train_data.txt -t sample/valid_data.txt
    Train iters 6/6 100.0%
    Train Spent 0s
    Train Finished in 0s
    Total Spent 0s
    Test iters 6/6 100.0%
    Test score 66.66%
    Test Spent 0s
    Test Finished in 0s
    Total Spent 0s

## Test

    $ ./bin/simhash-runner -f sample/v2-sample v2-test -s 'test data 1'
    [("label1",0.5322839697942492),("label2",0.46771603691875463)]


## Deploy Infer

    $ ./bin/simhash-runner -f sample/v2-sample v2-infer -H tcp://127.0.0.1:5000 -n v1-sample -w 10 -s 5


## Deploy Infer Learn

    $ ./bin/simhash-runner -f sample/v2-sample v2-infer-learn -H tcp://127.0.0.1:5000 -n v1-sample -w 10

## Custom model options

Option file name is `path/to/model_name.opts.yml`


    column_size: 1600
    encoder:
      size: 600
      sparsity: 0.2
      token_similarity: true
