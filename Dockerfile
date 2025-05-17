FROM haskell:9.8.4 as builder

# Install PostgreSQL 14 development libraries
RUN apt-get update && \
    apt-get install -y wget gnupg2 && \
    sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list' && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    apt-get update && \
    apt-get install -y libpq-dev postgresql-client-14 && \
    rm -rf /var/lib/apt/lists/*

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

# Install required system libraries
RUN apt-get update && \
    apt-get install -y libpq5 postgresql-client-14 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Create a non-root user
RUN useradd -m app
USER app

# Set working directory
WORKDIR /home/app

# Copy the built executable and required scripts
COPY --from=builder /app/.stack-work/dist/*/build/haskell-api/haskell-api ./
COPY --from=builder /app/migrations.sql ./
COPY --from=builder /app/start.sh ./

# Make start.sh executable
USER root
RUN chmod +x /home/app/start.sh
USER app

# Expose the port
EXPOSE 3000

# Run the application using start.sh
CMD ["./start.sh"] 