# An example of testing a bot

This example shows how one can test a bot using this library. The example bot is written in Rust to
demonstrate that this library can be used to test bots written in any programming language. The test
itself is in [`tests/Test.hs`](./tests/Test.hs).

## How to run

First, you need to compile the bot itself. For that, you will need to have Rust installed. To
compile the bot, run this command in the [`bot`](./bot/) directory:

```sh
cargo build --release
```

Once the bot compiles, you can run the test itself. Execute this command in this directory:

```sh
stack run example
```

For convenience, there's also a script named `run.sh` that compiles the bot and runs the test
automatically.
