{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
    ( User(..)
    , Employee(..)
    , EmploymentContract(..)
    , createUser
    , getUsers
    , getUserById
    , createEmployee
    , getEmployees
    , getEmployeeById
    , updateEmployee
    , deleteEmployee
    ) where

import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Text (Text)
import Data.Time (UTCTime, Day)

-- Employment contract type
data EmploymentContract = FullTime | PartTime
    deriving (Show, Generic, Eq)

instance ToJSON EmploymentContract where
    toJSON FullTime = String "FULL_TIME"
    toJSON PartTime = String "PART_TIME"

instance FromJSON EmploymentContract where
    parseJSON (String "FULL_TIME") = pure FullTime
    parseJSON (String "PART_TIME") = pure PartTime
    parseJSON _ = fail "Invalid employment contract type"

instance FromRow EmploymentContract where
    fromRow = do
        val <- field
        case val of
            "FULL_TIME" -> pure FullTime
            "PART_TIME" -> pure PartTime
            _ -> fail "Invalid employment contract type"

data User = User
    { userId :: Maybe Int
    , userName :: Text
    , userEmail :: Text
    } deriving (Show, Generic)

data Employee = Employee
    { employeeId :: Maybe Int
    , employeeFirstName :: Text
    , employeeMiddleName :: Maybe Text
    , employeeLastName :: Text
    , employeeDisplayName :: Text
    , employeeEmail :: Text
    , employeePosition :: Text
    , employeeAddress :: Text
    , employeeSite :: Text  -- city
    , employeeManagerId :: Maybe Int
    , employeeContract :: EmploymentContract
    , employeeStartDate :: Day
    , employeeEndDate :: Maybe Day
    , employeeDepartment :: Text
    , employeePictureUrl :: Maybe Text
    , employeeCreatedAt :: Maybe UTCTime
    } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
instance FromRow User

instance ToJSON Employee
instance FromJSON Employee
instance FromRow Employee

-- Database queries for Users
createUser :: Connection -> User -> IO [User]
createUser conn user = query conn
    "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email"
    (userName user, userEmail user)

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "SELECT id, name, email FROM users"

getUserById :: Connection -> Int -> IO [User]
getUserById conn uid = query conn
    "SELECT id, name, email FROM users WHERE id = ?"
    (Only uid)

-- Database queries for Employees
createEmployee :: Connection -> Employee -> IO [Employee]
createEmployee conn employee = query conn
    "INSERT INTO employees (first_name, middle_name, last_name, display_name, email, position, \
    \address, site, manager_id, employment_contract, start_date, end_date, department, picture_url) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \RETURNING id, first_name, middle_name, last_name, display_name, email, position, \
    \address, site, manager_id, employment_contract, start_date, end_date, department, picture_url, created_at"
    ( employeeFirstName employee
    , employeeMiddleName employee
    , employeeLastName employee
    , employeeDisplayName employee
    , employeeEmail employee
    , employeePosition employee
    , employeeAddress employee
    , employeeSite employee
    , employeeManagerId employee
    , show (employeeContract employee)
    , employeeStartDate employee
    , employeeEndDate employee
    , employeeDepartment employee
    , employeePictureUrl employee
    )

getEmployees :: Connection -> IO [Employee]
getEmployees conn = query_ conn 
    "SELECT id, first_name, middle_name, last_name, display_name, email, position, \
    \address, site, manager_id, employment_contract, start_date, end_date, department, picture_url, created_at \
    \FROM employees"

getEmployeeById :: Connection -> Int -> IO [Employee]
getEmployeeById conn eid = query conn
    "SELECT id, first_name, middle_name, last_name, display_name, email, position, \
    \address, site, manager_id, employment_contract, start_date, end_date, department, picture_url, created_at \
    \FROM employees WHERE id = ?"
    (Only eid)

updateEmployee :: Connection -> Int -> Employee -> IO [Employee]
updateEmployee conn eid employee = query conn
    "UPDATE employees \
    \SET first_name = ?, middle_name = ?, last_name = ?, display_name = ?, email = ?, position = ?, \
    \    address = ?, site = ?, manager_id = ?, employment_contract = ?, start_date = ?, \
    \    end_date = ?, department = ?, picture_url = ? \
    \WHERE id = ? \
    \RETURNING id, first_name, middle_name, last_name, display_name, email, position, \
    \address, site, manager_id, employment_contract, start_date, end_date, department, picture_url, created_at"
    ( employeeFirstName employee
    , employeeMiddleName employee
    , employeeLastName employee
    , employeeDisplayName employee
    , employeeEmail employee
    , employeePosition employee
    , employeeAddress employee
    , employeeSite employee
    , employeeManagerId employee
    , show (employeeContract employee)
    , employeeStartDate employee
    , employeeEndDate employee
    , employeeDepartment employee
    , employeePictureUrl employee
    , eid
    )

deleteEmployee :: Connection -> Int -> IO Bool
deleteEmployee conn eid = do
    n <- execute conn "DELETE FROM employees WHERE id = ?" (Only eid)
    return $ n > 0 