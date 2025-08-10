import React from 'react';
import { Link, useLocation } from 'react-router-dom';
import { Dumbbell, Users, Calendar, Activity, Home } from 'lucide-react';

const Navbar: React.FC = () => {
  const location = useLocation();

  const navItems = [
    { path: '/', label: 'Home', icon: Home },
    { path: '/users', label: 'Users', icon: Users },
    { path: '/exercises', label: 'Exercises', icon: Activity },
    { path: '/training-plans', label: 'Training Plans', icon: Calendar },
  ];

  return (
    <nav className="bg-white shadow-sm border-b border-gray-100 sticky top-0 z-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center h-16">
          <div className="flex items-center">
            <Link to="/" className="flex items-center space-x-3">
              <div className="w-10 h-10 bg-primary-600 rounded-xl flex items-center justify-center">
                <Dumbbell className="h-6 w-6 text-white" />
              </div>
              <div>
                <span className="text-xl font-bold text-gray-900">FitAndFun</span>
                <div className="text-xs text-gray-500">Gym Management</div>
              </div>
            </Link>
          </div>
          
          <div className="flex items-center space-x-1">
            {navItems.map(({ path, label, icon: Icon }) => (
              <Link
                key={path}
                to={path}
                className={`nav-link ${
                  location.pathname === path
                    ? 'nav-link-active'
                    : 'nav-link-inactive'
                }`}
              >
                <Icon className="h-4 w-4" />
                <span>{label}</span>
              </Link>
            ))}
          </div>
          
          <div className="flex items-center space-x-3">
            <div className="w-8 h-8 bg-gray-200 rounded-full flex items-center justify-center">
              <span className="text-sm font-medium text-gray-600">JD</span>
            </div>
          </div>
        </div>
      </div>
    </nav>
  );
};

export default Navbar;