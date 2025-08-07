# fit-and-fun-gym

A comprehensive gym management system built with Spring Boot that provides personal trainer-like functionality for creating and managing weekly training plans with exercises and diet plans.

## Features

### 🏋️‍♂️ Training Plans
- **Weekly Structure**: Training plans organized by days of the week (Monday-Sunday)
- **Daily Focus Areas**: Each day can have specific focus areas (Upper Body, Cardio, Rest Day, etc.)
- **Detailed Exercise Plans**: Each exercise includes sets, reps, weight, rest time, and special notes
- **Professional Layout**: Structured like what a personal trainer would give to their client

### 🥗 Nutrition Management
- **Daily Meal Plans**: Complete meal planning for each day
- **Meal Types**: Breakfast, Morning Snack, Lunch, Afternoon Snack, Dinner, Evening Snack
- **Nutritional Information**: Calories, protein, carbs, and fat calculations
- **Food Database**: Comprehensive food database with nutritional values

### 👥 User Management
- **Role-based Access**: USER, COACH, ADMIN roles
- **User Profiles**: Complete user information with training plan assignments
- **Training Plan Assignment**: Users can be assigned active training plans
- **Progress Tracking**: Track training plan status (ACTIVE/FINISHED)

### 🏃‍♀️ Exercise Database
- **Comprehensive Exercise Library**: Detailed exercise descriptions
- **Muscle Group Classification**: Exercises organized by target muscle groups
- **Equipment Requirements**: Track what equipment is needed for each exercise
- **Difficulty Levels**: Beginner, Intermediate, Advanced classifications

## New Model Structure

### Training Plan Hierarchy
```
TrainingPlan
├── DailyPlan (Monday-Sunday)
    ├── DailyExercise (with sets, reps, weight, rest time)
    └── DailyMeal (with quantities and nutritional info)
```

### Key Entities
- **TrainingPlan**: Overall weekly program with goal, difficulty, duration
- **DailyPlan**: Specific plan for each day of the week
- **DailyExercise**: Exercise with specific parameters (sets, reps, weight, rest)
- **DailyMeal**: Meal with food item, quantity, and meal type
- **Exercise**: Exercise database with muscle groups and equipment needs
- **Food**: Food database with nutritional information per 100g

## API Endpoints

### Weekly Plans
- `GET /project/weekly-plan/{trainingPlanId}` - Get complete weekly plan
- `GET /project/daily-plan/{trainingPlanId}/{dayOfWeek}` - Get specific day plan

### User Management
- `GET /project/users` - Get all users (paginated)
- `POST /project/users/addUser` - Create new user
- `POST /project/users/assignTrainingPlan` - Assign training plan to user
- `PUT /project/users/finishTrainingPlan/{userId}` - Mark training plan as finished

### Training Plans
- `GET /project/trainingPlans` - Get all training plans
- `POST /project/trainingPlans/add` - Create new training plan

### Exercises
- `GET /project/exercises` - Get all exercises
- `GET /project/exercises/findByMuscleGroup` - Find exercises by muscle group
- `POST /project/exercises/add` - Add new exercise

## Security
- Spring Security with role-based access control
- BCrypt password encoding
- Method-level security annotations

## Database
- PostgreSQL database
- JPA/Hibernate for ORM
- QueryDSL for complex queries
- Proper foreign key relationships and cascading

## Getting Started

1. **Database Setup**
   ```sql
   CREATE DATABASE FitAndFun;
   ```

2. **Application Properties**
   ```properties
   spring.datasource.url=jdbc:postgresql://localhost:5432/FitAndFun
   spring.datasource.username=postgres
   spring.datasource.password=postgres
   ```

3. **Run the Application**
   ```bash
   ./mvnw spring-boot:run
   ```

## Example Usage

A typical weekly plan would look like:
- **Monday**: Upper Body Focus - Bench Press, Rows, Shoulder Press + High Protein Meals
- **Tuesday**: Cardio Day - Running, Cycling + Balanced Nutrition
- **Wednesday**: Lower Body - Squats, Deadlifts, Lunges + Recovery Meals
- **Thursday**: Rest Day - Light Stretching + Maintenance Calories
- **Friday**: Full Body - Compound Movements + Pre-Weekend Prep
- **Saturday**: Functional Training + Flexible Eating
- **Sunday**: Active Recovery + Meal Prep Day

Each day includes specific exercises with sets/reps/weight and complete meal plans with calculated nutrition.
