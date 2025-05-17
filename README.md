# Haskell API

A simple REST API built with Scotty and PostgreSQL.

## Environment Variables

The application can be configured using the following environment variables:

### Server Configuration
- `PORT`: The port number for the server (default: 3000)

### Database Configuration
- `DB_HOST`: PostgreSQL host (default: localhost)
- `DB_PORT`: PostgreSQL port (default: 5432)
- `DB_USER`: PostgreSQL user (default: postgres)
- `DB_PASSWORD`: PostgreSQL password (default: postgres)
- `DB_NAME`: PostgreSQL database name (default: haskell_api)

## Running the Application

1. Make sure PostgreSQL is running
2. Create the database and tables:
   ```bash
   createdb haskell_api
   psql haskell_api -c "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL);"
   ```
3. Set environment variables as needed or use defaults
4. Run the application:
   ```bash
   stack run
   ```

## API Endpoints

- `GET /health`: Health check endpoint
- `GET /users`: List all users
- `GET /users/:id`: Get user by ID
- `POST /users`: Create a new user
- `GET /swagger.json`: API documentation
