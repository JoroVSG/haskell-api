import express from 'express';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const app = express();

// Health check endpoint
app.get('/health', (req, res) => {
    res.status(200).send('ok');
});

// Serve static files
app.use(express.static(join(__dirname, 'dist')));

// Handle SPA routing
app.get('*', (req, res) => {
    res.sendFile(join(__dirname, 'dist', 'index.html'));
});

const port = process.env.PORT || 80;
app.listen(port, () => {
    console.log(`Server is running on port ${port}`);
    console.log(`Health endpoint available at /health`);
    console.log(`Static files being served from ${join(__dirname, 'dist')}`);
}); 