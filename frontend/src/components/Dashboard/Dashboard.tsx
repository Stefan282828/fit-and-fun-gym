import React, { useState, useEffect } from 'react';
import { User, Exercise, TrainingPlan, PageResponse } from '../../types';
import { userApi, exerciseApi, trainingPlanApi } from '../../services/api';
import { Users, Activity, Calendar, TrendingUp } from 'lucide-react';
import { Link } from 'react-router-dom';

const Dashboard: React.FC = () => {
  const [stats, setStats] = useState({
    totalUsers: 0,
    totalExercises: 0,
    totalTrainingPlans: 0
  });
  const [recentUsers, setRecentUsers] = useState<User[]>([]);
  const [recentTrainingPlans, setRecentTrainingPlans] = useState<TrainingPlan[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchDashboardData();
  }, []);

  const fetchDashboardData = async () => {
    try {
      setLoading(true);
      
      // Fetch stats
      const [usersResponse, exercisesResponse, trainingPlansResponse] = await Promise.all([
        userApi.getAll(0, 1),
        exerciseApi.getAll(0, 1),
        trainingPlanApi.getAll(0, 1)
      ]);

      setStats({
        totalUsers: usersResponse.data.totalElements,
        totalExercises: exercisesResponse.data.totalElements,
        totalTrainingPlans: trainingPlansResponse.data.totalElements
      });

      // Fetch recent data
      const [recentUsersResponse, recentPlansResponse] = await Promise.all([
        userApi.getAll(0, 5),
        trainingPlanApi.getAll(0, 5)
      ]);

      setRecentUsers(recentUsersResponse.data.content);
      setRecentTrainingPlans(recentPlansResponse.data.content);
    } catch (error) {
      console.error('Error fetching dashboard data:', error);
    } finally {
      setLoading(false);
    }
  };

  const StatCard: React.FC<{
    title: string;
    value: number;
    icon: React.ReactNode;
    color: string;
    link: string;
  }> = ({ title, value, icon, color, link }) => (
    <Link to={link} className="block">
      <div className={`card hover:shadow-lg transition-shadow cursor-pointer border-l-4 ${color}`}>
        <div className="flex items-center justify-between">
          <div>
            <p className="text-sm font-medium text-gray-600">{title}</p>
            <p className="text-3xl font-bold text-gray-900">{value}</p>
          </div>
          <div className="text-primary-600">
            {icon}
          </div>
        </div>
      </div>
    </Link>
  );

  if (loading) {
    return <div className="flex justify-center items-center h-64">Loading dashboard...</div>;
  }

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-gray-900">Dashboard</h1>
        <p className="text-gray-600">Welcome to Fit & Fun Gym Management System</p>
      </div>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <StatCard
          title="Total Users"
          value={stats.totalUsers}
          icon={<Users className="h-8 w-8" />}
          color="border-blue-500"
          link="/users"
        />
        <StatCard
          title="Total Exercises"
          value={stats.totalExercises}
          icon={<Activity className="h-8 w-8" />}
          color="border-green-500"
          link="/exercises"
        />
        <StatCard
          title="Training Plans"
          value={stats.totalTrainingPlans}
          icon={<Calendar className="h-8 w-8" />}
          color="border-purple-500"
          link="/training-plans"
        />
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Recent Users */}
        <div className="card">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-xl font-bold text-gray-900">Recent Users</h2>
            <Link to="/users" className="text-primary-600 hover:text-primary-800 text-sm font-medium">
              View All
            </Link>
          </div>
          
          <div className="space-y-3">
            {recentUsers.map((user) => (
              <div key={user.id} className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                <div>
                  <div className="font-medium text-gray-900">
                    {user.name} {user.lastName}
                  </div>
                  <div className="text-sm text-gray-600">@{user.username}</div>
                </div>
                <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${
                  user.role === 'ADMIN' ? 'bg-red-100 text-red-800' :
                  user.role === 'COACH' ? 'bg-blue-100 text-blue-800' :
                  'bg-green-100 text-green-800'
                }`}>
                  {user.role}
                </span>
              </div>
            ))}
          </div>
        </div>

        {/* Recent Training Plans */}
        <div className="card">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-xl font-bold text-gray-900">Recent Training Plans</h2>
            <Link to="/training-plans" className="text-primary-600 hover:text-primary-800 text-sm font-medium">
              View All
            </Link>
          </div>
          
          <div className="space-y-3">
            {recentTrainingPlans.map((plan) => (
              <div key={plan.id} className="p-3 bg-gray-50 rounded-lg">
                <div className="flex items-start justify-between">
                  <div>
                    <div className="font-medium text-gray-900">{plan.name}</div>
                    <div className="text-sm text-gray-600">{plan.goal}</div>
                  </div>
                  <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${
                    plan.difficulty?.toLowerCase() === 'beginner' ? 'bg-green-100 text-green-800' :
                    plan.difficulty?.toLowerCase() === 'intermediate' ? 'bg-yellow-100 text-yellow-800' :
                    'bg-red-100 text-red-800'
                  }`}>
                    {plan.difficulty}
                  </span>
                </div>
                <div className="mt-2">
                  <Link
                    to={`/weekly-plan/${plan.id}`}
                    className="text-primary-600 hover:text-primary-800 text-sm font-medium"
                  >
                    View Weekly Plan →
                  </Link>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Quick Actions */}
      <div className="card">
        <h2 className="text-xl font-bold text-gray-900 mb-4">Quick Actions</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Link to="/users" className="btn-primary text-center">
            Add New User
          </Link>
          <Link to="/exercises" className="btn-primary text-center">
            Add New Exercise
          </Link>
          <Link to="/training-plans" className="btn-primary text-center">
            Create Training Plan
          </Link>
        </div>
      </div>
    </div>
  );
};

export default Dashboard;