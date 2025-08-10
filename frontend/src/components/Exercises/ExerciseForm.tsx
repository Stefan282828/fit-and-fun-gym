import React, { useState, useEffect } from 'react';
import { Exercise } from '../../types/index';
import { exerciseApi } from '../../services/api';
import { X } from 'lucide-react';

interface ExerciseFormProps {
  exercise?: Exercise | null;
  onClose: () => void;
  onSuccess: () => void;
}

const ExerciseForm: React.FC<ExerciseFormProps> = ({ exercise, onClose, onSuccess }) => {
  const [formData, setFormData] = useState<Exercise>({
    name: '',
    description: '',
    muscleGroup: '',
    equipmentNeeded: '',
    difficultyLevel: 'Beginner'
  });
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (exercise) {
      setFormData(exercise);
    }
  }, [exercise]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);

    try {
      await exerciseApi.create(formData);
      onSuccess();
    } catch (error) {
      console.error('Error saving exercise:', error);
      alert('Error saving exercise. Please try again.');
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
          <h2 className="text-xl font-bold text-gray-900">
            {exercise ? 'Edit Exercise' : 'Add New Exercise'}
          </h2>
          <button onClick={onClose} className="text-gray-400 hover:text-gray-600">
            <X className="h-6 w-6" />
          </button>
        </div>

        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Exercise Name
            </label>
            <input
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., Push-ups, Bench Press"
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
              placeholder="Detailed description of how to perform the exercise..."
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Muscle Group
            </label>
            <input
              type="text"
              name="muscleGroup"
              value={formData.muscleGroup}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., Chest, Back, Legs, Shoulders"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Equipment Needed
            </label>
            <input
              type="text"
              name="equipmentNeeded"
              value={formData.equipmentNeeded}
              onChange={handleChange}
              required
              className="input-field"
              placeholder="e.g., Dumbbells, Barbell, Bodyweight"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Difficulty Level
            </label>
            <select
              name="difficultyLevel"
              value={formData.difficultyLevel}
              onChange={handleChange}
              className="select-field"
            >
              <option value="Beginner">Beginner</option>
              <option value="Intermediate">Intermediate</option>
              <option value="Advanced">Advanced</option>
            </select>
          </div>

          <div className="flex space-x-3 pt-4">
            <button
              type="submit"
              disabled={loading}
              className="btn-primary flex-1 disabled:opacity-50"
            >
              {loading ? 'Saving...' : (exercise ? 'Update Exercise' : 'Create Exercise')}
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

export default ExerciseForm;