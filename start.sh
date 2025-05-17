#!/bin/bash

# Run migrations first
echo "Running database migrations..."
for file in migrations/V*__*.sql; do
    echo "Applying migration: $file"
    psql "$DATABASE_URL" -f "$file"
done

# Start the application
echo "Starting the application..."
./haskell-api 