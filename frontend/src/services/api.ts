import axios from 'axios';
import { User, UserRequest, Exercise, TrainingPlan, WeeklyPlan, DailyPlan, PageResponse } from '../types/index';

const API_BASE_URL = 'http://localhost:8080/project';

// Create axios instance with default config and basic auth
const api = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
  auth: {
    username: 'adminMaster',
    password: 'admin123'
  }
});

// User API
export const userApi = {
  getAll: (page = 0, size = 10, sortField = 'name', sortDirection = 'ASC') =>
    api.get<PageResponse<User>>(`/users?page=${page}&size=${size}&sortField=${sortField}&sortDirection=${sortDirection}`),
  
  getById: (id: number) =>
    api.get<User>(`/users/${id}`),
  
  create: (user: UserRequest) =>
    api.post<User>('/users/addUser', user),
  
  update: (user: UserRequest) =>
    api.put<User>('/users/update', user),
  
  delete: (id: number) =>
    api.delete(`/users/deleteById?id=${id}`),
  
  search: (user: Partial<UserRequest>, page = 0, size = 10, sortField = 'name', sortDirection = 'ASC') =>
    api.get<PageResponse<User>>(`/users/search?page=${page}&size=${size}&sortField=${sortField}&sortDirection=${sortDirection}`, {
      data: user
    }),
  
  assignTrainingPlan: (userId: number, trainingPlanId: number) =>
    api.post(`/users/assignTrainingPlan?userId=${userId}&trainingPlanId=${trainingPlanId}`),
  
  finishTrainingPlan: (userId: number) =>
    api.put(`/users/finishTrainingPlan/${userId}`)
};

// Exercise API
export const exerciseApi = {
  getAll: (page = 0, size = 10, sortField = 'name', sortDirection = 'ASC') =>
    api.get<PageResponse<Exercise>>(`/exercises?page=${page}&size=${size}&sortField=${sortField}&sortDirection=${sortDirection}`),
  
  getById: (id: number) =>
    api.get<Exercise>(`/exercises/${id}`),
  
  create: (exercise: Exercise) =>
    api.post<Exercise>('/exercises/add', exercise),
  
  delete: (id: number) =>
    api.delete(`/exercises/delete?id=${id}`),
  
  findByMuscleGroup: (muscleGroup: string) =>
    api.get<Exercise[]>(`/exercises/findByMuscleGroup?muscleGroup=${muscleGroup}`),
  
  getDescription: (exerciseName: string) =>
    api.get<string>(`/exercises/getExerciseDescription?exName=${exerciseName}`)
};

// Training Plan API
export const trainingPlanApi = {
  getAll: (page = 0, size = 10, sortField = 'name', sortDirection = 'ASC') =>
    api.get<PageResponse<TrainingPlan>>(`/trainingPlans?page=${page}&size=${size}&sortField=${sortField}&sortDirection=${sortDirection}`),
  
  getById: (id: number) =>
    api.get<TrainingPlan>(`/trainingPlans/${id}`),
  
  create: (trainingPlan: TrainingPlan) =>
    api.post<TrainingPlan>('/trainingPlans/add', trainingPlan)
};

// Weekly Plan API
export const weeklyPlanApi = {
  getWeeklyPlan: (trainingPlanId: number) =>
    api.get<WeeklyPlan>(`/weekly-plan/${trainingPlanId}`),
  
  getDailyPlan: (trainingPlanId: number, dayOfWeek: string) =>
    api.get<DailyPlan>(`/daily-plan/${trainingPlanId}/${dayOfWeek}`)
};

export default api;
