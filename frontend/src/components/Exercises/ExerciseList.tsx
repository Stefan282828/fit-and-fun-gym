import React, { useState, useEffect } from 'react';
import { Exercise, PageResponse } from '../../types';
import { exerciseApi } from '../../services/api';
import { Plus, Edit, Trash2, Search } from 'lucide-react';
import ExerciseForm from './ExerciseForm';

const ExerciseList: React.FC = () => {
  const [exercises, setExercises] = useState<PageResponse<Exercise>>({
    content: [],
    totalElements: 0,
    totalPages: 0,
    size: 10,
    number: 0
  });
  const [loading, setLoading] = useState(true);
  const [showForm, setShowForm] = useState(false);
  const [editingExercise, setEditingExercise] = useState<Exercise | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [muscleGroupFilter, setMuscleGroupFilter] = useState('');
  const [filteredExercises, setFilteredExercises] = useState<Exercise[]>([]);

  useEffect(() => {
    fetchExercises();
  }, [currentPage]);

  useEffect(() => {
    if (muscleGroupFilter) {
      searchByMuscleGroup();
    } else {
      setFilteredExercises(exercises.content);
    }
  }, [muscleGroupFilter, exercises.content]);

  const fetchExercises = async () => {
    try {
      setLoading(true);
      const response = await exerciseApi.getAll(currentPage, 10);
      setExercises(response.data);
    } catch (error) {
      console.error('Error fetching exercises:', error);
    } finally {
      setLoading(false);
    }
  };

  const searchByMuscleGroup = async () => {
    try {
      const response = await exerciseApi.findByMuscleGroup(muscleGroupFilter);
      setFilteredExercises(response.data);
    } catch (error) {
      console.error('Error searching exercises:', error);
    }
  };

  const handleDelete = async (id: number) => {
    if (window.confirm('Are you sure you want to delete this exercise?')) {
      try {
        await exerciseApi.delete(id);
        fetchExercises();
      } catch (error) {
        console.error('Error deleting exercise:', error);
      }
    }
  };

  const handleEdit = (exercise: Exercise) => {
    setEditingExercise(exercise);
    setShowForm(true);
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

  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 space-y-8">
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">Exercise Library</h1>
          <p className="text-gray-600 mt-1">Browse and manage workout exercises</p>
        </div>
        <button
          onClick={() => setShowForm(true)}
          className="btn-primary"
        >
          <Plus className="h-4 w-4" />
          <span>Add Exercise</span>
        </button>
      </div>

      {/* Search Filter */}
      <div className="card">
        <div className="flex items-end space-x-4">
          <div className="flex-1">
            <label className="block text-sm font-semibold text-gray-700 mb-2">
              Filter by Muscle Group
            </label>
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <input
                type="text"
                value={muscleGroupFilter}
                onChange={(e) => setMuscleGroupFilter(e.target.value)}
                placeholder="e.g., chest, back, legs..."
                className="input-field pl-10"
              />
            </div>
          </div>
          <button
            onClick={() => setMuscleGroupFilter('')}
            className="btn-secondary"
          >
            Clear Filter
          </button>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {filteredExercises.map((exercise) => (
          <div key={exercise.id} className="card group hover:shadow-soft transition-all duration-200">
            <div className="flex justify-between items-start mb-3">
              <h3 className="text-lg font-bold text-gray-900 group-hover:text-primary-600 transition-colors">{exercise.name}</h3>
              <div className="flex space-x-2">
                <button
                  onClick={() => handleEdit(exercise)}
                  className="text-primary-600 hover:text-primary-700 transition-colors"
                >
                  <Edit className="h-4 w-4" />
                </button>
                <button
                  onClick={() => handleDelete(exercise.id!)}
                  className="text-red-600 hover:text-red-700 transition-colors"
                >
                  <Trash2 className="h-4 w-4" />
                </button>
              </div>
            </div>

            <p className="text-gray-600 text-sm mb-4 line-clamp-3">
              {exercise.description}
            </p>

            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-sm font-semibold text-gray-700">Muscle Group:</span>
                <span className="text-sm text-gray-900 font-medium">{exercise.muscleGroup}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-semibold text-gray-700">Equipment:</span>
                <span className="text-sm text-gray-900 font-medium">{exercise.equipmentNeeded}</span>
              </div>
              
              <div className="flex items-center justify-between">
                <span className="text-sm font-semibold text-gray-700">Difficulty:</span>
                <span className={`inline-flex px-3 py-1 text-xs font-semibold rounded-full ${getDifficultyColor(exercise.difficultyLevel)}`}>
                  {exercise.difficultyLevel}
                </span>
              </div>
            </div>
          </div>
        ))}
      </div>

      {!muscleGroupFilter && (
        <div className="flex items-center justify-between bg-white rounded-xl p-4 shadow-card">
          <div className="text-sm text-gray-600">
            Showing {exercises.number * exercises.size + 1} to {Math.min((exercises.number + 1) * exercises.size, exercises.totalElements)} of {exercises.totalElements} results
          </div>
          <div className="flex space-x-2">
            <button
              onClick={() => setCurrentPage(Math.max(0, currentPage - 1))}
              disabled={currentPage === 0}
              className="btn-secondary disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Previous
            </button>
            <button
              onClick={() => setCurrentPage(Math.min(exercises.totalPages - 1, currentPage + 1))}
              disabled={currentPage >= exercises.totalPages - 1}
              className="btn-secondary disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Next
            </button>
          </div>
        </div>
      )}

      {showForm && (
        <ExerciseForm
          exercise={editingExercise}
          onClose={() => {
            setShowForm(false);
            setEditingExercise(null);
          }}
          onSuccess={() => {
            fetchExercises();
            setShowForm(false);
            setEditingExercise(null);
          }}
        />
      )}
    </div>
  );
};

export default ExerciseList;