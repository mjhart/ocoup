# OCaml Coup Game

A multiplayer implementation of the card game Coup, with an OCaml server and React client.

## Project Structure

- **server/**: OCaml server implementation
- **client/**: Next.js/React client implementation
- **.github/**: GitHub Actions workflows for CI/CD
- **Dockerfile**: For local development with the server
- **Dockerfile.deploy**: Used by GitHub Actions to create the deployment image

## Development Setup

### Prerequisites

- OCaml 5.1 with opam
- Node.js 18+ and npm
- Docker (optional)

### Running the Server Locally

```bash
# Navigate to the server directory
# Alternatively run all dune commands with --root=server
cd server

# Install dependencies
opam install . --deps-only

# Build the project
dune build

# Run the server
dune exec ocoup server
```

### Running the Client Locally

```bash
# Navigate to the client directory
cd client

# Install dependencies
npm install

# Run the development server
npm run dev
```

### Using Docker for the Server

```bash
# Build the Docker image
docker build -t ocoup-server .

# Run the container
docker run -p 8080:8080 ocoup-server
```

## CI/CD Pipeline

This project uses GitHub Actions for continuous integration and delivery:

1. On push to main, the server is built and tested
2. A Docker image is created and pushed to GitHub Container Registry
3. The image is tagged with the commit SHA and 'latest'

See the [CI/CD README](.github/README.md) for more details.

## License

[MIT License](LICENSE)