default: build

run *args:
  cargo run -- {{args}}

build:
  cargo build --release

test *args:
  NO_COLOR=1 cargo llvm-cov {{args}}
