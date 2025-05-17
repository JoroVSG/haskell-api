import { useState } from 'react';
import {
  AppBar,
  Box,
  Drawer,
  IconButton,
  List,
  ListItemIcon,
  ListItemText,
  Toolbar,
  Typography,
  useTheme,
  Tabs,
  Tab,
  Container,
  ListItemButton,
  alpha,
} from '@mui/material';
import {
  Menu as MenuIcon,
  Assessment as GoalsIcon,
  AdminPanelSettings as AdminIcon,
  Group as EmployeesIcon,
  Person as UsersIcon,
} from '@mui/icons-material';
import { Link as RouterLink, Outlet, useLocation } from 'react-router-dom';

const DRAWER_WIDTH = 280;

export function Layout() {
  const [drawerOpen, setDrawerOpen] = useState(false);
  const theme = useTheme();
  const location = useLocation();

  const currentSection = location.pathname.startsWith('/admin') ? 1 : 0;

  const handleDrawerToggle = () => {
    setDrawerOpen(!drawerOpen);
  };

  const adminDrawer = (
    <Box>
      <Toolbar />
      <List sx={{ px: 2 }}>
        <ListItemButton
          component={RouterLink}
          to="/admin"
          selected={location.pathname === '/admin' || location.pathname === '/admin/employees'}
          sx={{
            mb: 1,
            borderRadius: 2,
            '&.Mui-selected': {
              backgroundColor: alpha(theme.palette.primary.main, 0.08),
              '&:hover': {
                backgroundColor: alpha(theme.palette.primary.main, 0.12),
              },
              '& .MuiListItemIcon-root': {
                color: 'primary.main',
              },
              '& .MuiListItemText-primary': {
                color: 'primary.main',
                fontWeight: 600,
              },
            },
          }}
        >
          <ListItemIcon>
            <EmployeesIcon />
          </ListItemIcon>
          <ListItemText 
            primary="Employees"
            primaryTypographyProps={{
              fontSize: '0.9375rem',
              fontWeight: location.pathname === '/admin' || location.pathname === '/admin/employees' ? 600 : 500,
            }}
          />
        </ListItemButton>
        <ListItemButton
          component={RouterLink}
          to="/admin/users"
          selected={location.pathname === '/admin/users'}
          sx={{
            borderRadius: 2,
            '&.Mui-selected': {
              backgroundColor: alpha(theme.palette.primary.main, 0.08),
              '&:hover': {
                backgroundColor: alpha(theme.palette.primary.main, 0.12),
              },
              '& .MuiListItemIcon-root': {
                color: 'primary.main',
              },
              '& .MuiListItemText-primary': {
                color: 'primary.main',
                fontWeight: 600,
              },
            },
          }}
        >
          <ListItemIcon>
            <UsersIcon />
          </ListItemIcon>
          <ListItemText 
            primary="Users"
            primaryTypographyProps={{
              fontSize: '0.9375rem',
              fontWeight: location.pathname === '/admin/users' ? 600 : 500,
            }}
          />
        </ListItemButton>
      </List>
    </Box>
  );

  return (
    <Box sx={{ display: 'flex' }}>
      <AppBar position="fixed" sx={{ zIndex: theme.zIndex.drawer + 1 }}>
        <Toolbar sx={{ minHeight: { xs: 64, sm: 70 } }}>
          {currentSection === 1 && (
            <IconButton
              color="primary"
              aria-label="open drawer"
              edge="start"
              onClick={handleDrawerToggle}
              sx={{ mr: 2, display: { sm: 'none' } }}
            >
              <MenuIcon />
            </IconButton>
          )}
          <Typography 
            variant="h6" 
            component="div" 
            sx={{ 
              flexGrow: 0, 
              mr: 4, 
              fontSize: '1.125rem',
              fontWeight: 600,
              color: 'text.primary',
            }}
          >
            Company Portal
          </Typography>
          <Tabs 
            value={currentSection} 
            textColor="primary"
            sx={{ 
              '& .MuiTab-root': { 
                minHeight: { xs: 64, sm: 70 },
                fontSize: '0.9375rem',
                fontWeight: 500,
                color: 'text.secondary',
                '&.Mui-selected': { 
                  color: 'primary.main',
                  fontWeight: 600,
                },
                '& .MuiSvgIcon-root': {
                  fontSize: '1.25rem',
                  mr: 1,
                },
              },
              '& .MuiTabs-indicator': {
                height: 3,
                borderRadius: '3px 3px 0 0',
              },
            }}
          >
            <Tab
              icon={<GoalsIcon />}
              label="Goals"
              component={RouterLink}
              to="/"
              iconPosition="start"
            />
            <Tab
              icon={<AdminIcon />}
              label="Admin"
              component={RouterLink}
              to="/admin"
              iconPosition="start"
            />
          </Tabs>
        </Toolbar>
      </AppBar>
      {currentSection === 1 && (
        <>
          <Drawer
            variant="temporary"
            open={drawerOpen}
            onClose={handleDrawerToggle}
            ModalProps={{ keepMounted: true }}
            sx={{
              display: { xs: 'block', sm: 'none' },
              '& .MuiDrawer-paper': { 
                width: DRAWER_WIDTH,
                boxSizing: 'border-box',
                borderRight: 'none',
                boxShadow: '0px 10px 30px -5px rgba(0, 0, 0, 0.1)',
              },
            }}
          >
            {adminDrawer}
          </Drawer>
          <Drawer
            variant="permanent"
            sx={{
              display: { xs: 'none', sm: 'block' },
              width: DRAWER_WIDTH,
              flexShrink: 0,
              '& .MuiDrawer-paper': { 
                width: DRAWER_WIDTH,
                boxSizing: 'border-box',
                borderRight: 'none',
                boxShadow: '0px 10px 30px -5px rgba(0, 0, 0, 0.1)',
              },
            }}
            open
          >
            {adminDrawer}
          </Drawer>
        </>
      )}
      <Box
        component="main"
        sx={{
          flexGrow: 1,
          p: 3,
          width: { sm: `calc(100% - ${currentSection === 1 ? DRAWER_WIDTH : 0}px)` },
          ml: { sm: currentSection === 1 ? `${DRAWER_WIDTH}px` : 0 },
          backgroundColor: 'background.default',
          minHeight: '100vh',
        }}
      >
        <Toolbar sx={{ minHeight: { xs: 64, sm: 70 } }} />
        <Container maxWidth="lg">
          <Outlet />
        </Container>
      </Box>
    </Box>
  );
} 