FROM haskell:9.8.4-slim as builder

# Add retry logic for downloads
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    wget \
    gnupg2 \
    libpq-dev && \
    for i in {1..3}; do \
      wget --retry-connrefused --waitretry=1 --read-timeout=20 --timeout=15 --tries=3 \
        -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && break || sleep 5; \
    done && \
    echo "deb http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends postgresql-client-14 && \
    apt-get clean && \
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
RUN cp $(stack path --local-install-root)/bin/haskell-api .

# Start a new stage with a minimal image
FROM debian:bullseye-20240211-slim

# Install required system libraries with retry logic
RUN for i in {1..3}; do \
      (apt-get update && \
       apt-get install -y --no-install-recommends \
         ca-certificates \
         libpq5 \
         postgresql-client-14 && \
       apt-get clean && \
       rm -rf /var/lib/apt/lists/*) && break || \
      (echo "Retry attempt $i..." && sleep 5); \
    done

# Create a non-root user
RUN useradd -m app
USER app

# Set working directory
WORKDIR /home/app

# Copy the built executable and required scripts
COPY --from=builder /app/haskell-api ./

# Expose the port
EXPOSE 3000

# Run the application
CMD ["./haskell-api"] 