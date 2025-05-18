# Build stage
FROM node:20-alpine

WORKDIR /app

# Copy package files first
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy source code
COPY . ./

# Create production build
RUN npm run build

# Install express
RUN npm install express

# Add curl for debugging
RUN apk add --no-cache curl

# Expose port
EXPOSE 80

# Health check
HEALTHCHECK --interval=10s --timeout=3s --start-period=30s \
    CMD curl -f http://localhost:${PORT:-80}/health || exit 1

# Start server
CMD ["node", "server.js"] 