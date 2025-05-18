const express = require('express');
const path = require('path');
const app = express();

// Health check endpoint
app.get('/health', (req, res) => {
    res.status(200).send('ok');
});

// Serve static files
app.use(express.static(path.join(__dirname, 'dist')));

// Handle SPA routing
app.get('*', (req, res) => {
    res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

const port = process.env.PORT || 80;
app.listen(port, () => {
    console.log(`Server is running on port ${port}`);
    console.log(`Health endpoint available at /health`);
    console.log(`Static files being served from ${path.join(__dirname, 'dist')}`);
}); 