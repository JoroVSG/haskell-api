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

# Create a production directory
RUN mkdir -p /app/prod && \
    cp -r dist /app/prod/ && \
    cp server.js /app/prod/ && \
    cp package.json /app/prod/

# Switch to production directory
WORKDIR /app/prod

# Install only production dependencies
RUN npm install express --save && \
    npm install --production

# Add curl for debugging
RUN apk add --no-cache curl

# Expose port
EXPOSE 80

# Health check
HEALTHCHECK --interval=10s --timeout=3s --start-period=30s \
    CMD curl -f http://localhost:${PORT:-80}/health || exit 1

# Start server
CMD ["node", "server.js"] 