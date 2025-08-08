import React, { useState, useEffect } from 'react';
import { TrainingPlan, PageResponse } from '../../types';
import { trainingPlanApi } from '../../services/api';
import { Plus, Eye, Calendar } from 'lucide-react';
import TrainingPlanForm from './TrainingPlanForm';
import { Link } from 'react-router-dom';

const TrainingPlanList: React.FC = () => {
  const [trainingPlans, setTrainingPlans] = useState<PageResponse<TrainingPlan>>({
    content: [],
    totalElements: 0,
    totalPages: 0,
    size: 10,
    number: 0
  });
  const [loading, setLoading] = useState(true);
  const [showForm, setShowForm] = useState(false);
  const [currentPage, setCurrentPage] = useState(0);

  useEffect(() => {
    fetchTrainingPlans();
  }, [currentPage]);

  const fetchTrainingPlans = async () => {
    try {
      setLoading(true);
      const response = await trainingPlanApi.getAll(currentPage, 10);
      setTrainingPlans(response.data);
    } catch (error) {
      console.error('Error fetching training plans:', error);
    } finally {
      setLoading(false);
    }
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
    return <div className="flex justify-center items-center h-64">Loading...</div>;
  }

  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h1 className="text-3xl font-bold text-gray-900">Training Plans</h1>
        <button
          onClick={() => setShowForm(true)}
          className="btn-primary flex items-center space-x-2"
        >
          <Plus className="h-4 w-4" />
          <span>Create Training Plan</span>
        </button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {trainingPlans.content.map((plan) => (
          <div key={plan.id} className="card hover:shadow-lg transition-shadow">
            <div className="flex justify-between items-start mb-3">
              <h3 className="text-lg font-semibold text-gray-900">{plan.name}</h3>
              <Link
                to={`/weekly-plan/${plan.id}`}
                className="text-primary-600 hover:text-primary-900"
              >
                <Eye className="h-5 w-5" />
              </Link>
            </div>

            <p className="text-gray-600 text-sm mb-4 line-clamp-3">
              {plan.description}
            </p>

            <div className="space-y-3">
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Goal:</span>
                <span className="text-sm text-gray-900">{plan.goal}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Duration:</span>
                <span className="text-sm text-gray-900">{plan.duration}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-medium text-gray-700">Difficulty:</span>
                <span className={`inline-flex px-2 py-1 text-xs font-semibold rounded-full ${getDifficultyColor(plan.difficulty)}`}>
                  {plan.difficulty}
                </span>
              </div>
            </div>

            <div className="mt-4 pt-4 border-t border-gray-200">
              <Link
                to={`/weekly-plan/${plan.id}`}
                className="btn-primary w-full flex items-center justify-center space-x-2"
              >
                <Calendar className="h-4 w-4" />
                <span>View Weekly Plan</span>
              </Link>
            </div>
          </div>
        ))}
      </div>

      {/* Pagination */}
      <div className="flex items-center justify-between">
        <div className="text-sm text-gray-700">
          Showing {trainingPlans.number * trainingPlans.size + 1} to {Math.min((trainingPlans.number + 1) * trainingPlans.size, trainingPlans.totalElements)} of {trainingPlans.totalElements} results
        </div>
        <div className="flex space-x-2">
          <button
            onClick={() => setCurrentPage(Math.max(0, currentPage - 1))}
            disabled={currentPage === 0}
            className="btn-secondary disabled:opacity-50"
          >
            Previous
          </button>
          <button
            onClick={() => setCurrentPage(Math.min(trainingPlans.totalPages - 1, currentPage + 1))}
            disabled={currentPage >= trainingPlans.totalPages - 1}
            className="btn-secondary disabled:opacity-50"
          >
            Next
          </button>
        </div>
      </div>

      {showForm && (
        <TrainingPlanForm
          onClose={() => setShowForm(false)}
          onSuccess={() => {
            fetchTrainingPlans();
            setShowForm(false);
          }}
        />
      )}
    </div>
  );
};

export default TrainingPlanList;