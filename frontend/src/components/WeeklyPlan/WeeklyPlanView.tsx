import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { WeeklyPlan, DailyPlan } from '../../types';
import { weeklyPlanApi } from '../../services/api';
import { Calendar, Clock, Target, TrendingUp, ChevronDown, ChevronUp } from 'lucide-react';
import DailyPlanCard from './DailyPlanCard';

const WeeklyPlanView: React.FC = () => {
  const { trainingPlanId } = useParams<{ trainingPlanId: string }>();
  const [weeklyPlan, setWeeklyPlan] = useState<WeeklyPlan | null>(null);
  const [loading, setLoading] = useState(true);
  const [expandedDay, setExpandedDay] = useState<string | null>(null);

  const daysOfWeek = ['MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 'SATURDAY', 'SUNDAY'];
  const dayLabels: Record<string, string> = {
    MONDAY: 'Monday',
    TUESDAY: 'Tuesday',
    WEDNESDAY: 'Wednesday',
    THURSDAY: 'Thursday',
    FRIDAY: 'Friday',
    SATURDAY: 'Saturday',
    SUNDAY: 'Sunday'
  };

  useEffect(() => {
    if (trainingPlanId) {
      fetchWeeklyPlan();
    }
  }, [trainingPlanId]);

  const fetchWeeklyPlan = async () => {
    try {
      setLoading(true);
      const response = await weeklyPlanApi.getWeeklyPlan(Number(trainingPlanId));
      setWeeklyPlan(response.data);
    } catch (error) {
      console.error('Error fetching weekly plan:', error);
    } finally {
      setLoading(false);
    }
  };

  const toggleDay = (day: string) => {
    setExpandedDay(expandedDay === day ? null : day);
  };

  const getDifficultyColor = (difficulty: string) => {
    switch (difficulty?.toLowerCase()) {
      case 'beginner': return 'bg-green-100 text-green-800';
      case 'intermediate': return 'bg-yellow-100 text-yellow-800';
      case 'advanced': return 'bg-red-100 text-red-800';
      default: return 'bg-gray-100 text-gray-800';
    }
  };

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary-600"></div>
      </div>
    );
  }

  if (!weeklyPlan) {
    return (
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="text-center text-gray-600">Weekly plan not found.</div>
      </div>
    );
  }

  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 space-y-8">
      {/* Header */}
      <div className="card">
        <div className="flex items-start justify-between">
          <div>
            <h1 className="text-4xl font-bold text-gray-900 mb-3">
              {weeklyPlan.trainingPlanName}
            </h1>
            <p className="text-gray-600 mb-6 text-lg">{weeklyPlan.description}</p>
            
            <div className="flex flex-wrap gap-6">
              <div className="flex items-center space-x-2">
                <Target className="h-5 w-5 text-primary-600" />
                <span className="font-semibold text-gray-700">Goal:</span>
                <span className="text-gray-900 font-medium">{weeklyPlan.goal}</span>
              </div>
              
              <div className="flex items-center space-x-2">
                <Clock className="h-5 w-5 text-primary-600" />
                <span className="font-semibold text-gray-700">Duration:</span>
                <span className="text-gray-900 font-medium">{weeklyPlan.duration}</span>
              </div>
              
              <div className="flex items-center space-x-2">
                <TrendingUp className="h-5 w-5 text-primary-600" />
                <span className="font-semibold text-gray-700">Difficulty:</span>
                <span className={`inline-flex px-3 py-1 text-sm font-semibold rounded-full ${getDifficultyColor(weeklyPlan.difficulty)}`}>
                  {weeklyPlan.difficulty}
                </span>
              </div>
            </div>
          </div>
          
          <div className="w-16 h-16 bg-primary-100 rounded-2xl flex items-center justify-center">
            <Calendar className="h-8 w-8 text-primary-600" />
          </div>
        </div>
      </div>

      {/* Weekly Overview */}
      <div className="space-y-4">
        <div className="flex items-center space-x-3">
          <h2 className="text-2xl font-bold text-gray-900">Weekly Schedule</h2>
          <div className="h-1 flex-1 bg-gradient-to-r from-primary-600 to-primary-300 rounded-full"></div>
        </div>
        
        {daysOfWeek.map((day) => {
          const dailyPlan = weeklyPlan.dailyPlans[day];
          const isExpanded = expandedDay === day;
          
          return (
            <div key={day} className="card hover:shadow-soft transition-all duration-200">
              <div
                className="flex items-center justify-between cursor-pointer group"
                onClick={() => toggleDay(day)}
              >
                <div className="flex items-center space-x-4">
                  <div className="w-20 text-center">
                    <div className="text-lg font-bold text-primary-600 group-hover:text-primary-700 transition-colors">
                      {dayLabels[day]}
                    </div>
                  </div>
                  
                  <div>
                    <div className="text-lg font-bold text-gray-900 group-hover:text-primary-600 transition-colors">
                      {dailyPlan?.focusArea || 'Rest Day'}
                    </div>
                    {dailyPlan && (
                      <div className="text-gray-600">
                        {dailyPlan.exercises.length} exercises • {Object.keys(dailyPlan.meals).length} meal types
                      </div>
                    )}
                  </div>
                </div>
                
                <div className="w-8 h-8 bg-gray-100 rounded-lg flex items-center justify-center group-hover:bg-primary-100 transition-colors">
                  {isExpanded ? (
                    <ChevronUp className="h-4 w-4 text-gray-600 group-hover:text-primary-600" />
                  ) : (
                    <ChevronDown className="h-4 w-4 text-gray-600 group-hover:text-primary-600" />
                  )}
                </div>
              </div>
              
              {isExpanded && dailyPlan && (
                <div className="mt-8 pt-6 border-t border-gray-200">
                  <DailyPlanCard dailyPlan={dailyPlan} />
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default WeeklyPlanView;