FROM haskell:9.8.4 as builder

# Set working directory
WORKDIR /app

# Copy only package files first to cache dependencies
COPY package.yaml stack.yaml stack.yaml.lock ./
COPY haskell-api.cabal ./

# Install dependencies
RUN stack setup
RUN stack build --only-dependencies

# Copy the rest of the application
COPY . .

# Build the project
RUN stack build --system-ghc

# Start a new stage with a minimal image
FROM ubuntu:22.04

# Install required system libraries including PostgreSQL client
RUN apt-get update && \
    apt-get install -y libpq5 postgresql-client ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Create a non-root user
RUN useradd -m app
USER app

# Set working directory
WORKDIR /home/app

# Copy the built executable and migrations
COPY --from=builder /app/.stack-work/dist/*/build/haskell-api/haskell-api ./
COPY migrations.sql ./
COPY start.sh ./

# Make the startup script executable
RUN chmod +x start.sh

# Expose the port
EXPOSE 3000

# Run the startup script
CMD ["./start.sh"] 