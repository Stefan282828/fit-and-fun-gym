import React, { useState, useEffect } from 'react';
import { User, Exercise, TrainingPlan } from '../../types';
import { userApi, exerciseApi, trainingPlanApi } from '../../services/api';
import { Users, Activity, Calendar, TrendingUp, Star, ArrowRight, Play, Heart, Target, Zap } from 'lucide-react';
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

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary-600"></div>
      </div>
    );
  }

  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
        {/* Left Column - Main Content */}
        <div className="lg:col-span-2 space-y-8">
          {/* Hero Section */}
          <div className="card-hero relative overflow-hidden">
            <div className="relative z-10">
              <h1 className="text-4xl font-bold mb-4">
                Get Fit.<br />
                Stay Motivated.
              </h1>
              <p className="text-primary-100 mb-6 max-w-md">
                Transform your fitness journey with personalized training plans and expert guidance.
              </p>
              <Link to="/training-plans" className="inline-flex items-center bg-white text-primary-600 px-6 py-3 rounded-xl font-medium hover:bg-gray-50 transition-colors">
                Get Started
                <ArrowRight className="ml-2 h-4 w-4" />
              </Link>
            </div>
            <div className="absolute right-0 top-0 w-64 h-64 bg-primary-500 rounded-full opacity-20 transform translate-x-32 -translate-y-32"></div>
            <div className="absolute right-16 bottom-0 w-32 h-32 bg-primary-400 rounded-full opacity-30"></div>
          </div>

          {/* Stats Cards */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <Link to="/users" className="stat-card group">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-gray-600">Total Users</p>
                  <p className="text-3xl font-bold text-gray-900 mt-1">{stats.totalUsers}</p>
                </div>
                <div className="w-12 h-12 bg-blue-100 rounded-xl flex items-center justify-center group-hover:bg-blue-200 transition-colors">
                  <Users className="h-6 w-6 text-blue-600" />
                </div>
              </div>
            </Link>

            <Link to="/exercises" className="stat-card group">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-gray-600">Total Exercises</p>
                  <p className="text-3xl font-bold text-gray-900 mt-1">{stats.totalExercises}</p>
                </div>
                <div className="w-12 h-12 bg-green-100 rounded-xl flex items-center justify-center group-hover:bg-green-200 transition-colors">
                  <Activity className="h-6 w-6 text-green-600" />
                </div>
              </div>
            </Link>

            <Link to="/training-plans" className="stat-card group">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-gray-600">Training Plans</p>
                  <p className="text-3xl font-bold text-gray-900 mt-1">{stats.totalTrainingPlans}</p>
                </div>
                <div className="w-12 h-12 bg-purple-100 rounded-xl flex items-center justify-center group-hover:bg-purple-200 transition-colors">
                  <Calendar className="h-6 w-6 text-purple-600" />
                </div>
              </div>
            </Link>
          </div>

          {/* Features Section */}
          <div className="card">
            <h2 className="text-2xl font-bold text-gray-900 mb-6">Features</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
              <div className="text-center">
                <div className="feature-icon mx-auto">
                  <Target className="h-6 w-6" />
                </div>
                <h3 className="font-semibold text-gray-900 mb-2">Personalized Coaching</h3>
                <p className="text-sm text-gray-600">Tailored workout plans designed for your goals</p>
              </div>
              
              <div className="text-center">
                <div className="feature-icon mx-auto">
                  <Activity className="h-6 w-6" />
                </div>
                <h3 className="font-semibold text-gray-900 mb-2">Custom Workouts</h3>
                <p className="text-sm text-gray-600">Flexible exercise routines that adapt to you</p>
              </div>
              
              <div className="text-center">
                <div className="feature-icon mx-auto">
                  <TrendingUp className="h-6 w-6" />
                </div>
                <h3 className="font-semibold text-gray-900 mb-2">Progress Tracking</h3>
                <p className="text-sm text-gray-600">Monitor your fitness journey with detailed analytics</p>
              </div>
              
              <div className="text-center">
                <div className="feature-icon mx-auto">
                  <Heart className="h-6 w-6" />
                </div>
                <h3 className="font-semibold text-gray-900 mb-2">Community Support</h3>
                <p className="text-sm text-gray-600">Connect with trainers and fellow fitness enthusiasts</p>
              </div>
            </div>
          </div>

          {/* Testimonials */}
          <div className="card">
            <h2 className="text-2xl font-bold text-gray-900 mb-6">What Our Members Say</h2>
            <div className="space-y-6">
              <div className="flex items-start space-x-4">
                <div className="w-12 h-12 bg-gray-200 rounded-full flex items-center justify-center">
                  <span className="text-sm font-medium text-gray-600">SH</span>
                </div>
                <div className="flex-1">
                  <div className="flex items-center space-x-2 mb-2">
                    <h4 className="font-semibold text-gray-900">Sarah Henderson</h4>
                    <div className="flex space-x-1">
                      {[...Array(5)].map((_, i) => (
                        <Star key={i} className="h-4 w-4 text-yellow-400 fill-current" />
                      ))}
                    </div>
                  </div>
                  <p className="text-gray-600">
                    "The coaching and support have been fantastic! I've seen amazing progress since joining."
                  </p>
                </div>
              </div>
              
              <div className="flex items-start space-x-4">
                <div className="w-12 h-12 bg-gray-200 rounded-full flex items-center justify-center">
                  <span className="text-sm font-medium text-gray-600">MJ</span>
                </div>
                <div className="flex-1">
                  <div className="flex items-center space-x-2 mb-2">
                    <h4 className="font-semibold text-gray-900">Mike Johnson</h4>
                    <div className="flex space-x-1">
                      {[...Array(5)].map((_, i) => (
                        <Star key={i} className="h-4 w-4 text-yellow-400 fill-current" />
                      ))}
                    </div>
                  </div>
                  <p className="text-gray-600">
                    "Great workouts! I feel stronger every week with these personalized training plans."
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Right Column - Sidebar */}
        <div className="space-y-6">
          {/* User Profile Card */}
          <div className="card">
            <div className="flex items-center space-x-3 mb-4">
              <div className="w-12 h-12 bg-primary-100 rounded-full flex items-center justify-center">
                <span className="text-primary-600 font-semibold">JS</span>
              </div>
              <div>
                <h3 className="font-semibold text-gray-900">Jessica Smith</h3>
                <p className="text-sm text-gray-600">Age 28 • Female</p>
              </div>
            </div>
            <div className="space-y-3">
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Progress Tracker</span>
                <span className="text-sm font-medium text-primary-600">85% Complete</span>
              </div>
              <div className="w-full bg-gray-200 rounded-full h-2">
                <div className="bg-primary-600 h-2 rounded-full" style={{ width: '85%' }}></div>
              </div>
            </div>
          </div>

          {/* Training Plans List */}
          <div className="card">
            <div className="flex items-center justify-between mb-4">
              <h3 className="font-semibold text-gray-900">Training Plans</h3>
              <Link to="/training-plans" className="text-primary-600 text-sm font-medium hover:text-primary-700">
                View All
              </Link>
            </div>
            <div className="space-y-3">
              {recentTrainingPlans.slice(0, 3).map((plan) => (
                <div key={plan.id} className="flex items-center justify-between p-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition-colors">
                  <div className="flex items-center space-x-3">
                    <div className="w-8 h-8 bg-primary-100 rounded-lg flex items-center justify-center">
                      <Zap className="h-4 w-4 text-primary-600" />
                    </div>
                    <div>
                      <div className="font-medium text-gray-900 text-sm">{plan.name}</div>
                      <div className="text-xs text-gray-600">{plan.difficulty}</div>
                    </div>
                  </div>
                  <Link
                    to={`/weekly-plan/${plan.id}`}
                    className="text-primary-600 hover:text-primary-700"
                  >
                    <ArrowRight className="h-4 w-4" />
                  </Link>
                </div>
              ))}
            </div>
          </div>

          {/* Recent Users */}
          <div className="card">
            <div className="flex items-center justify-between mb-4">
              <h3 className="font-semibold text-gray-900">Recent Users</h3>
              <Link to="/users" className="text-primary-600 text-sm font-medium hover:text-primary-700">
                View All
              </Link>
            </div>
            <div className="space-y-3">
              {recentUsers.slice(0, 4).map((user) => (
                <div key={user.id} className="flex items-center space-x-3">
                  <div className="w-8 h-8 bg-gray-200 rounded-full flex items-center justify-center">
                    <span className="text-xs font-medium text-gray-600">
                      {user.name.charAt(0)}{user.lastName.charAt(0)}
                    </span>
                  </div>
                  <div className="flex-1">
                    <div className="font-medium text-gray-900 text-sm">
                      {user.name} {user.lastName}
                    </div>
                    <div className="text-xs text-gray-600">@{user.username}</div>
                  </div>
                  <span className={`inline-flex px-2 py-1 text-xs font-medium rounded-full ${
                    user.role === 'ADMIN' ? 'bg-red-100 text-red-700' :
                    user.role === 'COACH' ? 'bg-blue-100 text-blue-700' :
                    'bg-green-100 text-green-700'
                  }`}>
                    {user.role}
                  </span>
                </div>
              ))}
            </div>
          </div>

          {/* Quick Actions */}
          <div className="card">
            <h3 className="font-semibold text-gray-900 mb-4">Quick Actions</h3>
            <div className="space-y-2">
              <Link to="/users" className="w-full btn-secondary text-left flex items-center justify-between">
                <span>Add New User</span>
                <ArrowRight className="h-4 w-4" />
              </Link>
              <Link to="/exercises" className="w-full btn-secondary text-left flex items-center justify-between">
                <span>Add Exercise</span>
                <ArrowRight className="h-4 w-4" />
              </Link>
              <Link to="/training-plans" className="w-full btn-secondary text-left flex items-center justify-between">
                <span>Create Plan</span>
                <ArrowRight className="h-4 w-4" />
              </Link>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Dashboard;