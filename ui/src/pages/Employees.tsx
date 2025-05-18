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
    IconButton,
    Dialog,
    DialogTitle,
    DialogContent,
    DialogActions,
    TextField,
    MenuItem,
    Stack,
} from '@mui/material';
import { Edit as EditIcon, Delete as DeleteIcon, Add as AddIcon } from '@mui/icons-material';
import { employeeApi } from '../api/client';
import type { Employee, EmploymentContract } from '../types/api';

export function Employees() {
    const [open, setOpen] = useState(false);
    const [editingEmployee, setEditingEmployee] = useState<Employee | null>(null);
    const queryClient = useQueryClient();

    const { data: employees = [] } = useQuery({
        queryKey: ['employees'],
        queryFn: employeeApi.getAll,
    });

    const createMutation = useMutation({
        mutationFn: employeeApi.create,
        onSuccess: () => {
            queryClient.invalidateQueries({ queryKey: ['employees'] });
            setOpen(false);
        },
    });

    const updateMutation = useMutation({
        mutationFn: ({ id, employee }: { id: number; employee: Omit<Employee, 'employeeId' | 'employeeCreatedAt'> }) =>
            employeeApi.update(id, employee),
        onSuccess: () => {
            queryClient.invalidateQueries({ queryKey: ['employees'] });
            setOpen(false);
        },
    });

    const deleteMutation = useMutation({
        mutationFn: employeeApi.delete,
        onSuccess: () => {
            queryClient.invalidateQueries({ queryKey: ['employees'] });
        },
    });

    const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault();
        const formData = new FormData(event.currentTarget);
        const employeeData = {
            employeeFirstName: formData.get('firstName') as string,
            employeeMiddleName: formData.get('middleName') as string || null,
            employeeLastName: formData.get('lastName') as string,
            employeeDisplayName: formData.get('displayName') as string,
            employeeEmail: formData.get('email') as string,
            employeePosition: formData.get('position') as string,
            employeeAddress: formData.get('address') as string,
            employeeSite: formData.get('site') as string,
            employeeManagerId: null,
            employeeContract: formData.get('contract') as EmploymentContract,
            employeeStartDate: formData.get('startDate') as string,
            employeeEndDate: formData.get('endDate') as string || null,
            employeeDepartment: formData.get('department') as string,
            employeePictureUrl: null,
        };

        if (editingEmployee?.employeeId) {
            updateMutation.mutate({ id: editingEmployee.employeeId, employee: employeeData });
        } else {
            createMutation.mutate(employeeData);
        }
    };

    return (
        <>
            <Box sx={{ mb: 2 }}>
                <Button
                    variant="contained"
                    startIcon={<AddIcon />}
                    onClick={() => {
                        setEditingEmployee(null);
                        setOpen(true);
                    }}
                >
                    Add Employee
                </Button>
            </Box>

            <TableContainer component={Paper} sx={{ 
              width: '100%',
              margin: 0,
              '& .MuiTableCell-root': { 
                padding: '8px 16px',
              },
              '& .MuiTableCell-head': {
                fontWeight: 600,
              }
            }}>
                <Table size="small" sx={{ width: '100%', tableLayout: 'fixed' }}>
                    <TableHead>
                        <TableRow>
                            <TableCell width="20%">Name</TableCell>
                            <TableCell width="25%">Email</TableCell>
                            <TableCell width="20%">Position</TableCell>
                            <TableCell width="15%">Department</TableCell>
                            <TableCell width="10%">Contract</TableCell>
                            <TableCell width="10%">Actions</TableCell>
                        </TableRow>
                    </TableHead>
                    <TableBody>
                        {employees.map((employee) => (
                            <TableRow key={employee.employeeId}>
                                <TableCell>{employee.employeeDisplayName}</TableCell>
                                <TableCell>{employee.employeeEmail}</TableCell>
                                <TableCell>{employee.employeePosition}</TableCell>
                                <TableCell>{employee.employeeDepartment}</TableCell>
                                <TableCell>{employee.employeeContract}</TableCell>
                                <TableCell>
                                    <IconButton
                                        onClick={() => {
                                            setEditingEmployee(employee);
                                            setOpen(true);
                                        }}
                                    >
                                        <EditIcon />
                                    </IconButton>
                                    <IconButton
                                        onClick={() => {
                                            if (employee.employeeId) {
                                                deleteMutation.mutate(employee.employeeId);
                                            }
                                        }}
                                    >
                                        <DeleteIcon />
                                    </IconButton>
                                </TableCell>
                            </TableRow>
                        ))}
                    </TableBody>
                </Table>
            </TableContainer>

            <Dialog open={open} onClose={() => setOpen(false)} maxWidth="md" fullWidth>
                <form onSubmit={handleSubmit}>
                    <DialogTitle>
                        {editingEmployee ? 'Edit Employee' : 'Add Employee'}
                    </DialogTitle>
                    <DialogContent>
                        <Stack spacing={2} sx={{ mt: 1 }}>
                            <TextField
                                name="firstName"
                                label="First Name"
                                defaultValue={editingEmployee?.employeeFirstName || ''}
                                required
                            />
                            <TextField
                                name="middleName"
                                label="Middle Name"
                                defaultValue={editingEmployee?.employeeMiddleName || ''}
                            />
                            <TextField
                                name="lastName"
                                label="Last Name"
                                defaultValue={editingEmployee?.employeeLastName || ''}
                                required
                            />
                            <TextField
                                name="displayName"
                                label="Display Name"
                                defaultValue={editingEmployee?.employeeDisplayName || ''}
                                required
                            />
                            <TextField
                                name="email"
                                label="Email"
                                type="email"
                                defaultValue={editingEmployee?.employeeEmail || ''}
                                required
                            />
                            <TextField
                                name="position"
                                label="Position"
                                defaultValue={editingEmployee?.employeePosition || ''}
                                required
                            />
                            <TextField
                                name="address"
                                label="Address"
                                defaultValue={editingEmployee?.employeeAddress || ''}
                                required
                            />
                            <TextField
                                name="site"
                                label="Site"
                                defaultValue={editingEmployee?.employeeSite || ''}
                                required
                            />
                            <TextField
                                name="contract"
                                label="Contract Type"
                                select
                                defaultValue={editingEmployee?.employeeContract || 'FULL_TIME'}
                                required
                            >
                                <MenuItem value="FULL_TIME">Full Time</MenuItem>
                                <MenuItem value="PART_TIME">Part Time</MenuItem>
                            </TextField>
                            <TextField
                                name="startDate"
                                label="Start Date"
                                type="date"
                                defaultValue={editingEmployee?.employeeStartDate || new Date().toISOString().split('T')[0]}
                                required
                                InputLabelProps={{ shrink: true }}
                            />
                            <TextField
                                name="endDate"
                                label="End Date"
                                type="date"
                                defaultValue={editingEmployee?.employeeEndDate || ''}
                                InputLabelProps={{ shrink: true }}
                            />
                            <TextField
                                name="department"
                                label="Department"
                                defaultValue={editingEmployee?.employeeDepartment || ''}
                                required
                            />
                        </Stack>
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={() => setOpen(false)}>Cancel</Button>
                        <Button type="submit" variant="contained">
                            {editingEmployee ? 'Update' : 'Create'}
                        </Button>
                    </DialogActions>
                </form>
            </Dialog>
        </>
    );
} 