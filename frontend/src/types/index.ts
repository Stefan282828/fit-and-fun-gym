export interface User {
  id: number;
  name: string;
  lastName: string;
  username: string;
  email: string;
  dateOfBirth: string;
  role: 'USER' | 'COACH' | 'ADMIN';
}

export interface UserRequest {
  id?: number;
  name: string;
  lastName: string;
  username: string;
  email: string;
  password: string;
  dateOfBirth: string;
  role: 'USER' | 'COACH' | 'ADMIN';
}

export interface Exercise {
  id?: number;
  name: string;
  description: string;
  muscleGroup: string;
  equipmentNeeded: string;
  difficultyLevel: string;
}

export interface TrainingPlan {
  id?: number;
  name: string;
  goal: string;
  difficulty: string;
  duration: string;
  description: string;
  createdByCoachId?: number;
}

export interface DailyExercise {
  id: number;
  exerciseName: string;
  exerciseDescription: string;
  muscleGroup: string;
  equipmentNeeded: string;
  sets: number;
  reps: string;
  weight: string;
  restTime: string;
  orderInWorkout: number;
  notes: string;
}

export interface DailyMeal {
  id: number;
  foodName: string;
  foodDescription: string;
  foodCategory: string;
  mealType: 'BREAKFAST' | 'MORNING_SNACK' | 'LUNCH' | 'AFTERNOON_SNACK' | 'DINNER' | 'EVENING_SNACK';
  quantity: number;
  notes: string;
  totalCalories: number;
  totalProtein: number;
  totalCarbs: number;
  totalFat: number;
}

export interface DailyPlan {
  id: number;
  dayOfWeek: 'MONDAY' | 'TUESDAY' | 'WEDNESDAY' | 'THURSDAY' | 'FRIDAY' | 'SATURDAY' | 'SUNDAY';
  focusArea: string;
  notes: string;
  exercises: DailyExercise[];
  meals: Record<string, DailyMeal[]>;
}

export interface WeeklyPlan {
  trainingPlanId: number;
  trainingPlanName: string;
  goal: string;
  difficulty: string;
  duration: string;
  description: string;
  dailyPlans: Record<string, DailyPlan>;
}

export interface PageResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
}