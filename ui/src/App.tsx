import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { CssBaseline, ThemeProvider, createTheme } from '@mui/material';
import { Layout } from './components/Layout';
import { Employees } from './pages/Employees';
import { Users } from './pages/Users';

const queryClient = new QueryClient();

const theme = createTheme({
    palette: {
        mode: 'light',
        primary: {
            main: '#1976d2',
        },
    },
});

export function App() {
    return (
        <QueryClientProvider client={queryClient}>
            <ThemeProvider theme={theme}>
                <CssBaseline />
                <BrowserRouter>
                    <Routes>
                        <Route path="/" element={<Layout />}>
                            <Route index element={<Employees />} />
                            <Route path="users" element={<Users />} />
                            <Route path="*" element={<Navigate to="/" replace />} />
                        </Route>
                    </Routes>
                </BrowserRouter>
            </ThemeProvider>
        </QueryClientProvider>
    );
}
