FROM haskell:9.8.4 as builder

# Set working directory
WORKDIR /app

# Copy project files
COPY . .

# Build the project
RUN stack build --system-ghc

# Start a new stage with a minimal image
FROM ubuntu:22.04

# Install required system libraries
RUN apt-get update && \
    apt-get install -y libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy the built executable from builder
COPY --from=builder /app/.stack-work/dist/*/build/haskell-api/haskell-api .

# Set environment variables
ENV PORT=3000

# Expose the port
EXPOSE 3000

# Run the application
CMD ["./haskell-api"] 