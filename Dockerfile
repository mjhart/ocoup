# Build stage - using OCaml official image
FROM ocaml/opam:debian-12-ocaml-5.1 as builder

# Set working directory
WORKDIR /app

# Copy project files
COPY ./server /app/

# Install system dependencies
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    m4 \
    pkg-config \
    libssl-dev \
    libffi-dev \
    openssl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Switch back to opam user for OCaml operations
USER opam

# Create a new switch with the desired OCaml version
RUN opam update && opam switch create local-switch ocaml-base-compiler.5.1.0

# Set up environment variables for the switch
RUN eval $(opam env --switch=local-switch --set-switch)

# Install OCaml dependencies
RUN opam install -y dune.3.17 async async_ssl core cohttp-async yojson ppx_jane ppx_yojson_conv cohttp_async_websocket

# Build the project
RUN eval $(opam env) && dune build

# Runtime stage - using slim Debian
FROM debian:12-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl3 \
    libffi8 \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy the built executable from the builder stage
COPY --from=builder /app/_build/default/bin/main.exe /app/ocoup

# Make executable
RUN chmod +x /app/ocoup

# Expose WebSocket port
EXPOSE 8080

# Run the executable
ENTRYPOINT ["/app/ocoup", "server"]