import React, { useState } from 'react';
import { TrainingPlan } from '../../types';
import { trainingPlanApi } from '../../services/api';
import { X } from 'lucide-react';

interface TrainingPlanFormProps {
  onClose: () => void;
  onSuccess: () => void;
}

const TrainingPlanForm: React.FC<TrainingPlanFormProps> = ({ onClose, onSuccess }) => {
  const [formData, setFormData] = useState<TrainingPlan>({
    name: '',
    goal: '',
    difficulty: 'Beginner',
    duration: '',
    description: '',
    createdByCoachId: 1 // You might want to get this from user context
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);

    try {
      await trainingPlanApi.create(formData);
      onSuccess();
    } catch (error) {
      console.error('Error creating training plan:', error);
      alert('Error creating training plan. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
    setFormData({
      ...formData,
      [e.target.name]: e.target.value
    });
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg p-6 w-full max-w-md max-h-[90vh] overflow-y-auto">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-xl font-bold text-gray-900">Create Training Plan</h2>
          <button onClick={onClose} className="text-gray-400 hover:text-gray-600">
            <X className="h-6 w-6" />
          </button>
        </div>

        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Plan Name
            </label>
            <input
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., Beginner Full Body Workout"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Goal
            </label>
            <input
              type="text"
              name="goal"
              value={formData.goal}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., Weight Loss, Muscle Building, Strength"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Difficulty Level
            </label>
            <select
              name="difficulty"
              value={formData.difficulty}
              onChange={handleChange}
              className="select-field"
            >
              <option value="Beginner">Beginner</option>
              <option value="Intermediate">Intermediate</option>
              <option value="Advanced">Advanced</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Duration
            </label>
            <input
              type="text"
              name="duration"
              value={formData.duration}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., 4 weeks, 8 weeks, 12 weeks"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Description
            </label>
            <textarea
              name="description"
              value={formData.description}
              onChange={handleChange}
              required
              rows={4}
              className="input-field"
              placeholder="Detailed description of the training plan..."
            />
          </div>

          <div className="flex space-x-3 pt-4">
            <button
              type="submit"
              disabled={loading}
              className="btn-primary flex-1 disabled:opacity-50"
            >
              {loading ? 'Creating...' : 'Create Training Plan'}
            </button>
            <button
              type="button"
              onClick={onClose}
              className="btn-secondary flex-1"
            >
              Cancel
            </button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default TrainingPlanForm;