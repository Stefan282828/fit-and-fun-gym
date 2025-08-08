import React from 'react';
import { DailyPlan, DailyExercise, DailyMeal } from '../../types';
import { Activity, Utensils, Clock, Weight, RotateCcw } from 'lucide-react';

interface DailyPlanCardProps {
  dailyPlan: DailyPlan;
}

const DailyPlanCard: React.FC<DailyPlanCardProps> = ({ dailyPlan }) => {
  const mealTypeLabels: Record<string, string> = {
    BREAKFAST: 'Breakfast',
    MORNING_SNACK: 'Morning Snack',
    LUNCH: 'Lunch',
    AFTERNOON_SNACK: 'Afternoon Snack',
    DINNER: 'Dinner',
    EVENING_SNACK: 'Evening Snack'
  };

  const calculateTotalNutrition = (meals: DailyMeal[]) => {
    return meals.reduce(
      (total, meal) => ({
        calories: total.calories + (meal.totalCalories || 0),
        protein: total.protein + (meal.totalProtein || 0),
        carbs: total.carbs + (meal.totalCarbs || 0),
        fat: total.fat + (meal.totalFat || 0)
      }),
      { calories: 0, protein: 0, carbs: 0, fat: 0 }
    );
  };

  const allMeals = Object.values(dailyPlan.meals).flat();
  const totalNutrition = calculateTotalNutrition(allMeals);

  return (
    <div className="space-y-6">
      {dailyPlan.notes && (
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h4 className="font-medium text-blue-900 mb-2">Daily Notes</h4>
          <p className="text-blue-800 text-sm">{dailyPlan.notes}</p>
        </div>
      )}

      {/* Exercises Section */}
      {dailyPlan.exercises.length > 0 && (
        <div>
          <div className="flex items-center space-x-2 mb-4">
            <Activity className="h-5 w-5 text-primary-600" />
            <h3 className="text-lg font-semibold text-gray-900">Exercises</h3>
          </div>
          
          <div className="space-y-4">
            {dailyPlan.exercises.map((exercise: DailyExercise, index) => (
              <div key={exercise.id} className="bg-gray-50 rounded-lg p-4">
                <div className="flex items-start justify-between mb-2">
                  <div>
                    <h4 className="font-medium text-gray-900">
                      {index + 1}. {exercise.exerciseName}
                    </h4>
                    <p className="text-sm text-gray-600 mt-1">{exercise.exerciseDescription}</p>
                  </div>
                  <span className="text-xs bg-primary-100 text-primary-800 px-2 py-1 rounded">
                    {exercise.muscleGroup}
                  </span>
                </div>
                
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-3">
                  <div className="flex items-center space-x-2">
                    <span className="text-sm font-medium text-gray-700">Sets:</span>
                    <span className="text-sm text-gray-900">{exercise.sets}</span>
                  </div>
                  
                  <div className="flex items-center space-x-2">
                    <span className="text-sm font-medium text-gray-700">Reps:</span>
                    <span className="text-sm text-gray-900">{exercise.reps}</span>
                  </div>
                  
                  <div className="flex items-center space-x-2">
                    <Weight className="h-4 w-4 text-gray-500" />
                    <span className="text-sm text-gray-900">{exercise.weight}</span>
                  </div>
                  
                  <div className="flex items-center space-x-2">
                    <Clock className="h-4 w-4 text-gray-500" />
                    <span className="text-sm text-gray-900">{exercise.restTime}</span>
                  </div>
                </div>
                
                {exercise.equipmentNeeded && (
                  <div className="mt-2">
                    <span className="text-xs text-gray-600">Equipment: {exercise.equipmentNeeded}</span>
                  </div>
                )}
                
                {exercise.notes && (
                  <div className="mt-2 p-2 bg-yellow-50 border border-yellow-200 rounded">
                    <span className="text-xs text-yellow-800">{exercise.notes}</span>
                  </div>
                )}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Meals Section */}
      {Object.keys(dailyPlan.meals).length > 0 && (
        <div>
          <div className="flex items-center justify-between mb-4">
            <div className="flex items-center space-x-2">
              <Utensils className="h-5 w-5 text-primary-600" />
              <h3 className="text-lg font-semibold text-gray-900">Nutrition Plan</h3>
            </div>
            
            {/* Daily Nutrition Summary */}
            <div className="bg-green-50 border border-green-200 rounded-lg p-3">
              <div className="text-xs text-green-800 font-medium mb-1">Daily Totals</div>
              <div className="grid grid-cols-4 gap-2 text-xs text-green-700">
                <div>{Math.round(totalNutrition.calories)}cal</div>
                <div>{Math.round(totalNutrition.protein)}g protein</div>
                <div>{Math.round(totalNutrition.carbs)}g carbs</div>
                <div>{Math.round(totalNutrition.fat)}g fat</div>
              </div>
            </div>
          </div>
          
          <div className="space-y-4">
            {Object.entries(dailyPlan.meals).map(([mealType, meals]) => (
              <div key={mealType} className="bg-gray-50 rounded-lg p-4">
                <h4 className="font-medium text-gray-900 mb-3">
                  {mealTypeLabels[mealType] || mealType}
                </h4>
                
                <div className="space-y-2">
                  {meals.map((meal: DailyMeal) => (
                    <div key={meal.id} className="bg-white rounded p-3 border border-gray-200">
                      <div className="flex items-start justify-between">
                        <div>
                          <div className="font-medium text-gray-900">{meal.foodName}</div>
                          <div className="text-sm text-gray-600">{meal.quantity}g</div>
                          {meal.notes && (
                            <div className="text-xs text-gray-500 mt-1">{meal.notes}</div>
                          )}
                        </div>
                        
                        <div className="text-right">
                          <div className="text-sm font-medium text-gray-900">
                            {Math.round(meal.totalCalories || 0)} cal
                          </div>
                          <div className="text-xs text-gray-600">
                            P: {Math.round(meal.totalProtein || 0)}g • 
                            C: {Math.round(meal.totalCarbs || 0)}g • 
                            F: {Math.round(meal.totalFat || 0)}g
                          </div>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
};

export default DailyPlanCard;