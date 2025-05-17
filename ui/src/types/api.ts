export type EmploymentContract = 'FULL_TIME' | 'PART_TIME';

export interface Employee {
    employeeId?: number;
    employeeFirstName: string;
    employeeMiddleName?: string | null;
    employeeLastName: string;
    employeeDisplayName: string;
    employeeEmail: string;
    employeePosition: string;
    employeeAddress: string;
    employeeSite: string;
    employeeManagerId?: number | null;
    employeeContract: EmploymentContract;
    employeeStartDate: string;
    employeeEndDate?: string | null;
    employeeDepartment: string;
    employeePictureUrl?: string | null;
    employeeCreatedAt?: string;
}

export interface User {
    userId?: number;
    userName: string;
    userEmail: string;
}

export interface HealthResponse {
    status: string;
} 