#!/usr/bin/env sh

cd `dirname "$0"`/bot
cargo build --release
cd ..
stack run example
