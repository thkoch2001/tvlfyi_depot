FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder
RUN apt update -q && apt install -qy protobuf-compiler
COPY --from=planner /app/recipe.json recipe.json
ENV PROTO_ROOT=/app
# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
# Needed for protoc
COPY . tvix
RUN cargo build --release --bin nar-bridge

# We do not need the Rust toolchain to run the binary!
FROM debian:bookworm-slim AS runtime
WORKDIR /app
COPY --from=builder /app/target/release/nar-bridge /usr/local/bin
ENTRYPOINT ["/usr/local/bin/nar-bridge"]
