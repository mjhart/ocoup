# OCaml Coup Game Server CI/CD

This directory contains GitHub Actions workflows for building and deploying the OCaml Coup game server.

## CI/CD Setup

### GitHub Actions Workflow

The GitHub Actions workflow in `workflows/build.yml` does the following:

1. **Build Job**:
   - Checks out the code
   - Sets up OCaml environment
   - Installs project dependencies
   - Builds the OCaml server
   - Runs tests
   - Uploads the built executable as an artifact

2. **Docker Build Job**:
   - Only runs on pushes to main branch
   - Downloads the built executable artifact
   - Builds a Docker image using `Dockerfile.deploy`
   - Pushes the Docker image to GitHub Container Registry (GHCR)

### Required Secrets

The workflow uses the following GitHub secrets:

- `GITHUB_TOKEN` (automatically provided): Used for pushing to GHCR

### Docker Images

Two Dockerfiles are provided:

1. **Dockerfile**: For local development and testing. This file:
   - Builds the OCaml server from source
   - Creates a lightweight runtime image

2. **Dockerfile.deploy**: Used by GitHub Actions. This file:
   - Uses the pre-built executable from the build job
   - Creates a minimal runtime image

## Usage

### Running the Container Locally

To build and run the server locally:

```bash
# Build the image
docker build -t ocoup-server .

# Run the container
docker run -p 8080:8080 ocoup-server
```

### Using the GitHub Container Registry Image

After a successful workflow run, you can pull and run the image from GitHub Container Registry:

```bash
# Pull the latest image
docker pull ghcr.io/OWNER/ocoup-server:latest

# Run the container
docker run -p 8080:8080 ghcr.io/OWNER/ocoup-server:latest
```

Replace `OWNER` with your GitHub username or organization name.

## Deployment

The workflow doesn't include automatic deployment to production. To deploy the image to your hosting platform, you can:

1. Add additional deployment jobs to the workflow
2. Manually pull and deploy the image from GHCR
3. Set up webhooks to trigger deployments when new images are published

## Troubleshooting

If the workflow fails:

1. Check that all dependencies are correctly specified in the project's opam file
2. Verify that the OCaml version is compatible (currently using 5.1.x)
3. Check the build logs for specific error messages
4. Ensure you have the necessary permissions for pushing to GHCR