# OCaml Coup Game

A multiplayer implementation of the card game Coup, with an OCaml server and React client.

## Project Structure

- **server/**: OCaml server implementation
- **client/**: Next.js/React client implementation
- **.github/**: GitHub Actions workflows for CI/CD
- **Dockerfile**: For local development with the server
- **Dockerfile.deploy**: Used by GitHub Actions to create the deployment image
- **fly.toml**: Configuration for deploying to Fly.io

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

## Deployment

### Server Deployment to Fly.io

The server can be deployed to Fly.io using the following steps:

1. Install the Fly CLI:
   ```bash
   curl -L https://fly.io/install.sh | sh
   ```

2. Log in to Fly:
   ```bash
   fly auth login
   ```

3. Update the `fly.toml` file:
   - Change `USERNAME` to your GitHub username
   - Update the region if needed

4. Deploy the application:
   ```bash
   fly launch --image ghcr.io/USERNAME/ocoup-server:latest
   ```

5. For subsequent deployments:
   ```bash
   fly deploy
   ```

### Client Deployment

The client can be deployed to Vercel or any other hosting service that supports Next.js:

```bash
# Navigate to the client directory
cd client

# Build the project
npm run build

# For Vercel deployment
vercel
```

## CI/CD Pipeline

This project uses GitHub Actions for continuous integration and delivery:

1. On push to main, the server is built and tested
2. A Docker image is created and pushed to GitHub Container Registry
3. The image is tagged with the commit SHA and 'latest'

See the [CI/CD README](.github/README.md) for more details.

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Commit your changes: `git commit -m 'Add feature'`
4. Push to the branch: `git push origin feature-name`
5. Submit a pull request

## License

[MIT License](LICENSE)