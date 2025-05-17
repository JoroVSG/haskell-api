-- V2: Create employees table
CREATE TABLE IF NOT EXISTS employees (
    id SERIAL PRIMARY KEY,
    first_name TEXT NOT NULL,
    middle_name TEXT,
    last_name TEXT NOT NULL,
    display_name TEXT NOT NULL,
    email TEXT NOT NULL,
    position TEXT NOT NULL,
    address TEXT NOT NULL,
    site TEXT NOT NULL,  -- city
    manager_id INTEGER REFERENCES employees(id),
    employment_contract TEXT NOT NULL CHECK (employment_contract IN ('FULL_TIME', 'PART_TIME')),
    start_date DATE NOT NULL,
    end_date DATE,
    department TEXT NOT NULL,
    picture_url TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Add unique constraint on employee email
ALTER TABLE employees ADD CONSTRAINT employees_email_unique UNIQUE (email);

-- Add index on manager_id for better query performance
CREATE INDEX IF NOT EXISTS idx_employees_manager_id ON employees(manager_id); 