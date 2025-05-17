#!/bin/bash

# Function to wait for PostgreSQL
wait_for_postgres() {
    echo "Waiting for PostgreSQL to be ready..."
    while ! pg_isready -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" >/dev/null 2>&1; do
        echo "PostgreSQL is unavailable - sleeping"
        sleep 1
    done
    echo "PostgreSQL is up and running!"
}

# Wait for PostgreSQL to be ready
wait_for_postgres

# Run database migrations
echo "Running database migrations..."
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" -f migrations.sql

# Start the application
echo "Starting the application..."
./haskell-api 