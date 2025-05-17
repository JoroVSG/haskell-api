import axios from 'axios';
import type { Employee, User, HealthResponse } from '../types/api';

const API_URL = import.meta.env.VITE_API_URL || 'http://localhost:3000';

const api = axios.create({
    baseURL: API_URL,
    headers: {
        'Content-Type': 'application/json',
    },
});

export const employeeApi = {
    getAll: () => api.get<Employee[]>('/employees').then(res => res.data),
    getById: (id: number) => api.get<Employee>(`/employees/${id}`).then(res => res.data),
    create: (employee: Omit<Employee, 'employeeId' | 'employeeCreatedAt'>) => 
        api.post<Employee>('/employees', employee).then(res => res.data),
    update: (id: number, employee: Omit<Employee, 'employeeId' | 'employeeCreatedAt'>) => 
        api.put<Employee>(`/employees/${id}`, employee).then(res => res.data),
    delete: (id: number) => api.delete(`/employees/${id}`),
};

export const userApi = {
    getAll: () => api.get<User[]>('/users').then(res => res.data),
    getById: (id: number) => api.get<User>(`/users/${id}`).then(res => res.data),
    create: (user: Omit<User, 'userId'>) => 
        api.post<User>('/users', user).then(res => res.data),
};

export const systemApi = {
    health: () => api.get<HealthResponse>('/health').then(res => res.data),
}; 