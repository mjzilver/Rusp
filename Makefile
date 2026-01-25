.PHONY: build run test integration-test clean

build:
	cargo build

run:
	cargo run

test:
	cargo test --lib

integration-test:
	RUSTFLAGS='--cfg test' cargo test --test integration_tests --features test-helpers

clean:
	cargo clean
