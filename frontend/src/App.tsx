import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Layout from './components/Layout/Layout';
import Dashboard from './components/Dashboard/Dashboard';
import UserList from './components/Users/UserList';
import ExerciseList from './components/Exercises/ExerciseList';
import TrainingPlanList from './components/TrainingPlans/TrainingPlanList';
import WeeklyPlanView from './components/WeeklyPlan/WeeklyPlanView';

function App() {
  return (
    <Router>
      <Layout>
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/users" element={<UserList />} />
          <Route path="/exercises" element={<ExerciseList />} />
          <Route path="/training-plans" element={<TrainingPlanList />} />
          <Route path="/weekly-plan/:trainingPlanId" element={<WeeklyPlanView />} />
        </Routes>
      </Layout>
    </Router>
  );
}

export default App;