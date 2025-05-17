-- Create users table if it doesn't exist
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL
);

-- Add unique constraint on email
ALTER TABLE users ADD CONSTRAINT users_email_unique UNIQUE (email); 