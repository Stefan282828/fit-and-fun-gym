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
    return <div className="flex justify-center items-center h-64">Loading weekly plan...</div>;
  }

  if (!weeklyPlan) {
    return <div className="text-center text-gray-600">Weekly plan not found.</div>;
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="card">
        <div className="flex items-start justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">
              {weeklyPlan.trainingPlanName}
            </h1>
            <p className="text-gray-600 mb-4">{weeklyPlan.description}</p>
            
            <div className="flex flex-wrap gap-4">
              <div className="flex items-center space-x-2">
                <Target className="h-5 w-5 text-primary-600" />
                <span className="text-sm font-medium text-gray-700">Goal:</span>
                <span className="text-sm text-gray-900">{weeklyPlan.goal}</span>
              </div>
              
              <div className="flex items-center space-x-2">
                <Clock className="h-5 w-5 text-primary-600" />
                <span className="text-sm font-medium text-gray-700">Duration:</span>
                <span className="text-sm text-gray-900">{weeklyPlan.duration}</span>
              </div>
              
              <div className="flex items-center space-x-2">
                <TrendingUp className="h-5 w-5 text-primary-600" />
                <span className="text-sm font-medium text-gray-700">Difficulty:</span>
                <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${getDifficultyColor(weeklyPlan.difficulty)}`}>
                  {weeklyPlan.difficulty}
                </span>
              </div>
            </div>
          </div>
          
          <Calendar className="h-12 w-12 text-primary-600" />
        </div>
      </div>

      {/* Weekly Overview */}
      <div className="space-y-4">
        <h2 className="text-2xl font-bold text-gray-900">Weekly Schedule</h2>
        
        {daysOfWeek.map((day) => {
          const dailyPlan = weeklyPlan.dailyPlans[day];
          const isExpanded = expandedDay === day;
          
          return (
            <div key={day} className="card">
              <div
                className="flex items-center justify-between cursor-pointer"
                onClick={() => toggleDay(day)}
              >
                <div className="flex items-center space-x-4">
                  <div className="w-16 text-center">
                    <div className="text-lg font-bold text-gray-900">{dayLabels[day]}</div>
                  </div>
                  
                  <div>
                    <div className="text-lg font-semibold text-gray-900">
                      {dailyPlan?.focusArea || 'Rest Day'}
                    </div>
                    {dailyPlan && (
                      <div className="text-sm text-gray-600">
                        {dailyPlan.exercises.length} exercises • {Object.keys(dailyPlan.meals).length} meal types
                      </div>
                    )}
                  </div>
                </div>
                
                {isExpanded ? (
                  <ChevronUp className="h-5 w-5 text-gray-400" />
                ) : (
                  <ChevronDown className="h-5 w-5 text-gray-400" />
                )}
              </div>
              
              {isExpanded && dailyPlan && (
                <div className="mt-6 pt-6 border-t border-gray-200">
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