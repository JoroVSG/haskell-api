#!/bin/bash

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to log messages
log() {
    local level=$1
    local message=$2
    local color=$NC
    
    case $level in
        "INFO") color=$GREEN;;
        "WARN") color=$YELLOW;;
        "ERROR") color=$RED;;
    esac
    
    echo -e "${color}[$level] $message${NC}"
}

# Function to check database connection
check_database_connection() {
    log "INFO" "Checking database connection..."
    if ! psql "$DATABASE_URL" -c '\q' 2>/dev/null; then
        log "ERROR" "Could not connect to database. Please check your DATABASE_URL"
        exit 1
    fi
    log "INFO" "Database connection successful"
}

# Function to run a migration file
run_migration() {
    local file=$1
    local version=$(basename "$file" | cut -d'_' -f1)
    local description=$(basename "$file" | cut -d'_' -f2- | sed 's/\.sql$//')
    
    log "INFO" "Checking migration ${version} (${description})..."
    
    # Check if migration has already been applied
    if psql "$DATABASE_URL" -t -c "SELECT version FROM schema_migrations WHERE version = '$version';" | grep -q "$version"; then
        log "INFO" "Migration $version already applied"
        return 0
    fi
    
    log "INFO" "Applying migration $version..."
    
    # Start a transaction
    psql "$DATABASE_URL" <<EOF
    BEGIN;
    
    -- Run the migration
    \i $file
    
    -- Record the migration
    INSERT INTO schema_migrations (version) VALUES ('$version');
    
    COMMIT;
EOF
    
    if [ $? -eq 0 ]; then
        log "INFO" "Migration $version applied successfully"
        return 0
    else
        log "ERROR" "Error applying migration $version"
        return 1
    fi
}

# Function to list applied migrations
list_migrations() {
    log "INFO" "Listing applied migrations:"
    psql "$DATABASE_URL" -c "SELECT version, applied_at FROM schema_migrations ORDER BY applied_at;"
}

# Ensure DATABASE_URL is set
if [ -z "$DATABASE_URL" ]; then
    log "ERROR" "DATABASE_URL environment variable is not set"
    exit 1
fi

# Create migrations directory if it doesn't exist
mkdir -p migrations

# Command line argument parsing
case "${1:-apply}" in
    "apply")
        check_database_connection
        # Run migrations in order
        for migration in $(ls migrations/V*__*.sql | sort); do
            if ! run_migration "$migration"; then
                log "ERROR" "Migration failed. Stopping."
                exit 1
            fi
        done
        log "INFO" "All migrations completed successfully"
        list_migrations
        ;;
    "list")
        check_database_connection
        list_migrations
        ;;
    "status")
        check_database_connection
        log "INFO" "Checking migration status:"
        for migration in $(ls migrations/V*__*.sql | sort); do
            version=$(basename "$migration" | cut -d'_' -f1)
            description=$(basename "$migration" | cut -d'_' -f2- | sed 's/\.sql$//')
            if psql "$DATABASE_URL" -t -c "SELECT version FROM schema_migrations WHERE version = '$version';" | grep -q "$version"; then
                log "INFO" "✓ $version - $description (applied)"
            else
                log "WARN" "✗ $version - $description (pending)"
            fi
        done
        ;;
    *)
        log "ERROR" "Unknown command: $1"
        log "INFO" "Usage: $0 [apply|list|status]"
        exit 1
        ;;
esac 