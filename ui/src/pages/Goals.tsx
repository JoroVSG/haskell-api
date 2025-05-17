import {
  Box,
  Card,
  CardContent,
  Grid,
  Typography,
  LinearProgress,
  IconButton,
  Paper,
  alpha,
} from '@mui/material';
import {
  TrendingUp as GrowthIcon,
  EmojiEvents as AchievementIcon,
  Timeline as ProgressIcon,
  MoreVert as MoreIcon,
  ArrowUpward as IncreaseIcon,
} from '@mui/icons-material';

// Mock data - replace with actual API data later
const companyGoals = [
  {
    id: 1,
    title: 'Revenue Growth',
    description: 'Achieve $10M in annual revenue',
    progress: 65,
    trend: '+10%',
    icon: <GrowthIcon fontSize="large" />,
    gradient: 'linear-gradient(135deg, #22c55e 0%, #15803d 100%)',
    color: '#22c55e',
  },
  {
    id: 2,
    title: 'Customer Satisfaction',
    description: 'Maintain 95% satisfaction rate',
    progress: 80,
    trend: '+5%',
    icon: <AchievementIcon fontSize="large" />,
    gradient: 'linear-gradient(135deg, #8b5cf6 0%, #6d28d9 100%)',
    color: '#8b5cf6',
  },
  {
    id: 3,
    title: 'Market Expansion',
    description: 'Enter 3 new market segments',
    progress: 45,
    trend: '+2',
    icon: <ProgressIcon fontSize="large" />,
    gradient: 'linear-gradient(135deg, #ec4899 0%, #be185d 100%)',
    color: '#ec4899',
  },
];

export function Goals() {
  return (
    <Box>
      <Typography variant="h4" gutterBottom sx={{ mb: 4, color: 'text.primary' }}>
        Company Goals & Objectives
      </Typography>

      <Grid container spacing={3}>
        {companyGoals.map((goal) => (
          // @ts-ignore - MUI Grid typing issue
          <Grid key={goal.id} item xs={12} md={4}>
            <Card 
              sx={{ 
                height: '100%',
                position: 'relative',
                overflow: 'hidden',
                backdropFilter: 'blur(20px)',
                transition: 'all 0.3s ease-in-out',
                '&:hover': {
                  transform: 'translateY(-4px)',
                  '& .goal-icon': {
                    transform: 'scale(1.1)',
                  },
                },
              }}
            >
              <Box
                sx={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  right: 0,
                  height: '100%',
                  opacity: 0.08,
                  background: goal.gradient,
                }}
              />
              <CardContent sx={{ position: 'relative' }}>
                <Box sx={{ display: 'flex', justifyContent: 'space-between', alignItems: 'flex-start', mb: 3 }}>
                  <Box 
                    className="goal-icon"
                    sx={{ 
                      p: 1.5,
                      borderRadius: 2,
                      background: goal.gradient,
                      color: '#fff',
                      transition: 'transform 0.3s ease-in-out',
                    }}
                  >
                    {goal.icon}
                  </Box>
                  <Box sx={{ textAlign: 'right' }}>
                    <Typography 
                      variant="subtitle2" 
                      sx={{ 
                        display: 'flex', 
                        alignItems: 'center', 
                        color: 'success.main',
                        mb: 0.5,
                      }}
                    >
                      <IncreaseIcon sx={{ fontSize: 16, mr: 0.5 }} />
                      {goal.trend}
                    </Typography>
                    <IconButton size="small">
                      <MoreIcon />
                    </IconButton>
                  </Box>
                </Box>

                <Typography variant="h6" gutterBottom>
                  {goal.title}
                </Typography>
                <Typography variant="body2" color="text.secondary" sx={{ mb: 3 }}>
                  {goal.description}
                </Typography>

                <Box>
                  <Box sx={{ display: 'flex', justifyContent: 'space-between', mb: 1 }}>
                    <Typography variant="body2" color="text.secondary">
                      Progress
                    </Typography>
                    <Typography 
                      variant="body2" 
                      sx={{ 
                        fontWeight: 600,
                        color: (theme) => alpha(goal.color, 0.9),
                      }}
                    >
                      {goal.progress}%
                    </Typography>
                  </Box>
                  <LinearProgress 
                    variant="determinate" 
                    value={goal.progress} 
                    sx={{ 
                      height: 8,
                      borderRadius: 4,
                      backgroundColor: (theme) => alpha(goal.color, 0.12),
                      '& .MuiLinearProgress-bar': {
                        background: goal.gradient,
                      },
                    }}
                  />
                </Box>
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>

      <Paper 
        sx={{ 
          mt: 4, 
          p: 3,
          background: 'linear-gradient(135deg, rgba(255,255,255,0.9) 0%, rgba(255,255,255,0.95) 100%)',
          backdropFilter: 'blur(20px)',
          border: '1px solid',
          borderColor: 'divider',
        }}
      >
        <Typography variant="h6" gutterBottom color="text.primary">
          Recent Updates
        </Typography>
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1.5 }}>
          <Typography 
            variant="body2" 
            color="text.secondary"
            sx={{ 
              display: 'flex',
              alignItems: 'center',
              '&::before': {
                content: '""',
                width: 6,
                height: 6,
                borderRadius: '50%',
                backgroundColor: 'primary.main',
                marginRight: 1.5,
              },
            }}
          >
            Q3 revenue targets exceeded by 15%
          </Typography>
          <Typography 
            variant="body2" 
            color="text.secondary"
            sx={{ 
              display: 'flex',
              alignItems: 'center',
              '&::before': {
                content: '""',
                width: 6,
                height: 6,
                borderRadius: '50%',
                backgroundColor: 'secondary.main',
                marginRight: 1.5,
              },
            }}
          >
            Employee satisfaction survey completed with 85% participation
          </Typography>
          <Typography 
            variant="body2" 
            color="text.secondary"
            sx={{ 
              display: 'flex',
              alignItems: 'center',
              '&::before': {
                content: '""',
                width: 6,
                height: 6,
                borderRadius: '50%',
                backgroundColor: 'error.main',
                marginRight: 1.5,
              },
            }}
          >
            Successfully launched in 2 new market segments
          </Typography>
        </Box>
      </Paper>
    </Box>
  );
} 