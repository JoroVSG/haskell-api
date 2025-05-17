import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
    Box,
    Button,
    Paper,
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
    Dialog,
    DialogTitle,
    DialogContent,
    DialogActions,
    TextField,
    Stack,
} from '@mui/material';
import { Add as AddIcon } from '@mui/icons-material';
import { userApi } from '../api/client';
import type { User } from '../types/api';

export function Users() {
    const [open, setOpen] = useState(false);
    const queryClient = useQueryClient();

    const { data: users = [] } = useQuery({
        queryKey: ['users'],
        queryFn: userApi.getAll,
    });

    const createMutation = useMutation({
        mutationFn: userApi.create,
        onSuccess: () => {
            queryClient.invalidateQueries({ queryKey: ['users'] });
            setOpen(false);
        },
    });

    const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault();
        const formData = new FormData(event.currentTarget);
        const userData = {
            userName: formData.get('name') as string,
            userEmail: formData.get('email') as string,
        };
        createMutation.mutate(userData);
    };

    return (
        <>
            <Box sx={{ mb: 2 }}>
                <Button
                    variant="contained"
                    startIcon={<AddIcon />}
                    onClick={() => setOpen(true)}
                >
                    Add User
                </Button>
            </Box>

            <TableContainer component={Paper} sx={{ 
              width: '100%',
              margin: 0,
              '& .MuiTableCell-root': { 
                padding: '8px 16px',
              },
            }}>
                <Table size="small" sx={{ width: '100%', tableLayout: 'fixed' }}>
                    <TableHead>
                        <TableRow>
                            <TableCell width="50%">Name</TableCell>
                            <TableCell width="50%">Email</TableCell>
                        </TableRow>
                    </TableHead>
                    <TableBody>
                        {users.map((user) => (
                            <TableRow key={user.userId}>
                                <TableCell>{user.userName}</TableCell>
                                <TableCell>{user.userEmail}</TableCell>
                            </TableRow>
                        ))}
                    </TableBody>
                </Table>
            </TableContainer>

            <Dialog open={open} onClose={() => setOpen(false)} maxWidth="sm" fullWidth>
                <form onSubmit={handleSubmit}>
                    <DialogTitle>Add User</DialogTitle>
                    <DialogContent>
                        <Stack spacing={2} sx={{ mt: 1 }}>
                            <TextField
                                name="name"
                                label="Name"
                                required
                            />
                            <TextField
                                name="email"
                                label="Email"
                                type="email"
                                required
                            />
                        </Stack>
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={() => setOpen(false)}>Cancel</Button>
                        <Button type="submit" variant="contained">
                            Create
                        </Button>
                    </DialogActions>
                </form>
            </Dialog>
        </>
    );
} 