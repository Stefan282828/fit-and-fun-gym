import React, { useState, useEffect } from 'react';
import { TrainingPlan, PageResponse } from '../../types';
import { trainingPlanApi, userApi } from '../../services/api';
import { X } from 'lucide-react';

interface AssignTrainingPlanModalProps {
  userId: number;
  onClose: () => void;
  onSuccess: () => void;
}

const AssignTrainingPlanModal: React.FC<AssignTrainingPlanModalProps> = ({
  userId,
  onClose,
  onSuccess
}) => {
  const [trainingPlans, setTrainingPlans] = useState<TrainingPlan[]>([]);
  const [selectedPlanId, setSelectedPlanId] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    fetchTrainingPlans();
  }, []);

  const fetchTrainingPlans = async () => {
    try {
      const response = await trainingPlanApi.getAll(0, 100);
      setTrainingPlans(response.data.content);
    } catch (error) {
      console.error('Error fetching training plans:', error);
    }
  };

  const handleAssign = async () => {
    if (!selectedPlanId) {
      alert('Please select a training plan');
      return;
    }

    setLoading(true);
    try {
      await userApi.assignTrainingPlan(userId, selectedPlanId);
      alert('Training plan assigned successfully!');
      onSuccess();
    } catch (error) {
      console.error('Error assigning training plan:', error);
      alert('Error assigning training plan. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg p-6 w-full max-w-md">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-xl font-bold text-gray-900">
            Assign Training Plan
          </h2>
          <button onClick={onClose} className="text-gray-400 hover:text-gray-600">
            <X className="h-6 w-6" />
          </button>
        </div>

        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Select Training Plan
            </label>
            <div className="space-y-2 max-h-64 overflow-y-auto">
              {trainingPlans.map((plan) => (
                <div
                  key={plan.id}
                  className={`p-3 border rounded-lg cursor-pointer transition-colors ${
                    selectedPlanId === plan.id
                      ? 'border-primary-500 bg-primary-50'
                      : 'border-gray-200 hover:border-gray-300'
                  }`}
                  onClick={() => setSelectedPlanId(plan.id!)}
                >
                  <div className="font-medium text-gray-900">{plan.name}</div>
                  <div className="text-sm text-gray-600">{plan.goal}</div>
                  <div className="text-xs text-gray-500">
                    {plan.difficulty} • {plan.duration}
                  </div>
                </div>
              ))}
            </div>
          </div>

          <div className="flex space-x-3 pt-4">
            <button
              onClick={handleAssign}
              disabled={loading || !selectedPlanId}
              className="btn-primary flex-1 disabled:opacity-50"
            >
              {loading ? 'Assigning...' : 'Assign Plan'}
            </button>
            <button
              onClick={onClose}
              className="btn-secondary flex-1"
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AssignTrainingPlanModal;