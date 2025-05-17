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

# Create startup script
COPY --chown=app:app <<'EOF' /home/app/start.sh
#!/bin/bash
# Wait for PostgreSQL to be ready
until PGPASSWORD=$PGPASSWORD psql -h "$PGHOST" -U "$PGUSER" -d "$PGDATABASE" -c '\q'; do
  echo "Postgres is unavailable - sleeping"
  sleep 1
done

echo "Postgres is up - running migrations"
PGPASSWORD=$PGPASSWORD psql -h "$PGHOST" -U "$PGUSER" -d "$PGDATABASE" -f migrations.sql

echo "Starting application"
exec ./haskell-api
EOF

RUN chmod +x /home/app/start.sh

# Expose the port
EXPOSE 3000

# Run the startup script
CMD ["./start.sh"] 