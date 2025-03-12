# Use Ubuntu 24.04 to match GitHub Actions environment
FROM ubuntu:24.04

# Install runtime dependencies 
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl3 \
    libffi8 \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy the prebuilt executable from the build process
COPY ./artifacts/main.exe /app/ocoup

# Make executable
RUN chmod +x /app/ocoup

# Expose WebSocket port
EXPOSE 8080

# Run the executable
ENTRYPOINT ["/app/ocoup", "server"]