# 🏋️ Fit and Fun Gym

A full-stack gym management system that works like a personal trainer — creating and managing weekly training plans, diet plans, exercises, and user profiles. Built with a **Java Spring Boot** backend and a **React + TypeScript** frontend.

---

## 📌 Overview

Fit and Fun Gym allows coaches and admins to:
- Build structured weekly training plans with day-by-day exercise and meal breakdowns
- Maintain a searchable exercise and food database
- Manage gym members, assign training plans, and track plan completion status

---

## 🛠️ Tech Stack

### Backend
| Technology | Purpose |
|---|---|
| **Java 17** | Primary backend language |
| **Spring Boot 3.4** | Application framework |
| **Spring Security** | Authentication & role-based access control |
| **Spring Data JPA / Hibernate** | ORM and database interaction |
| **QueryDSL 5** | Type-safe dynamic queries |
| **PostgreSQL** | Relational database |
| **Maven** | Build and dependency management |

### Frontend
| Technology | Purpose |
|---|---|
| **TypeScript** | Primary frontend language |
| **React 19** | UI framework |
| **React Router 7** | Client-side routing |
| **Tailwind CSS 4** | Utility-first styling |
| **Axios** | HTTP client for API calls |
| **Lucide React** | Icon library |

---

## ✅ Implemented Features

### 👥 User Management
- Create users with name, username, email, date of birth, and role
- Roles: `USER`, `COACH`, `ADMIN`
- Assign training plans to users
- Mark training plans as finished (`ACTIVE` → `FINISHED`)
- Paginated user list

### 🏋️ Exercise Database
- Add exercises with descriptions, muscle group, equipment needed, and difficulty level (`Beginner`, `Intermediate`, `Advanced`)
- Search/filter exercises by muscle group

### 📋 Training Plans
- Create weekly training plans with a goal, difficulty, and duration
- Each plan has a `DailyPlan` for every day of the week (Monday–Sunday)
- Each `DailyPlan` contains:
  - **DailyExercise** entries: exercise, sets, reps, weight, rest time, notes
  - **DailyMeal** entries: food item, quantity, meal type (Breakfast, Lunch, Dinner, snacks, etc.)

### 🥗 Nutrition / Meal Plans
- Food database with nutritional values per 100g (calories, protein, carbs, fat)
- Meal types: `BREAKFAST`, `MORNING_SNACK`, `LUNCH`, `AFTERNOON_SNACK`, `DINNER`, `EVENING_SNACK`
- Per-day nutritional totals calculated automatically

### 📅 Weekly Plan View
- Full weekly overview per training plan
- Day-by-day drill-down with exercises and meals side by side

### 🔒 Security
- Spring Security with BCrypt password encoding
- Role-based access control on API endpoints

---

## 🗂️ Project Structure

```
fit-and-fun-gym/
├── src/main/java/project/FitAndFunGym/
│   ├── config/          # Security configuration
│   ├── controller/      # REST controllers (User, Exercise, TrainingPlan, WeeklyPlan)
│   ├── dto/             # Request/Response DTOs
│   ├── entity/          # JPA entities
│   ├── exception/       # Global exception handling
│   ├── mapper/          # Entity ↔ DTO mappers
│   ├── repository/      # Spring Data JPA repositories
│   ├── service/         # Business logic
│   ├── util/            # Shared utilities
│   └── validator/       # Input validation
└── frontend/
    └── src/
        ├── components/
        │   ├── Dashboard/       # Home dashboard
        │   ├── Exercises/       # Exercise list & form
        │   ├── Layout/          # Navbar & page layout
        │   ├── TrainingPlans/   # Training plan list & form
        │   ├── Users/           # User list, form, assign-plan modal
        │   └── WeeklyPlan/      # Weekly & daily plan views
        ├── services/            # Axios API service layer
        └── types/               # TypeScript type definitions
```

### Data Model Hierarchy
```
TrainingPlan
└── DailyPlan (one per day: Monday–Sunday)
    ├── DailyExercise (exercise + sets/reps/weight/rest/notes)
    └── DailyMeal (food item + quantity + meal type)

User
└── UserTrainingPlan (many-to-many, with status ACTIVE/FINISHED)
```

---

## 🌐 API Endpoints

### Users — `/project/users`
| Method | Path | Description |
|---|---|---|
| `GET` | `/project/users` | Get all users (paginated) |
| `POST` | `/project/users/addUser` | Create a new user |
| `POST` | `/project/users/assignTrainingPlan` | Assign a training plan to a user |
| `PUT` | `/project/users/finishTrainingPlan/{userId}` | Mark active training plan as finished |

### Exercises — `/project/exercises`
| Method | Path | Description |
|---|---|---|
| `GET` | `/project/exercises` | Get all exercises |
| `GET` | `/project/exercises/findByMuscleGroup` | Filter by muscle group |
| `POST` | `/project/exercises/add` | Add a new exercise |

### Training Plans — `/project/trainingPlans`
| Method | Path | Description |
|---|---|---|
| `GET` | `/project/trainingPlans` | Get all training plans |
| `POST` | `/project/trainingPlans/add` | Create a new training plan |

### Weekly Plans — `/project`
| Method | Path | Description |
|---|---|---|
| `GET` | `/project/weekly-plan/{trainingPlanId}` | Get the full weekly plan |
| `GET` | `/project/daily-plan/{trainingPlanId}/{dayOfWeek}` | Get a specific day's plan |

---

## 🚀 Getting Started

### Prerequisites
- Java 17+
- Maven
- PostgreSQL
- Node.js 16+ and npm (for the frontend)

### 1. Database Setup

```sql
CREATE DATABASE FitAndFun;
```

### 2. Configure Backend

Edit `src/main/resources/application.properties`:

```properties
spring.datasource.url=jdbc:postgresql://localhost:5432/FitAndFun
spring.datasource.username=postgres
spring.datasource.password=postgres
spring.jpa.hibernate.ddl-auto=update
```

### 3. Run the Backend

```bash
./mvnw spring-boot:run
```

The API will be available at `http://localhost:8080`.

### 4. Run the Frontend

```bash
cd frontend
npm install
npm start
```

The app will open at `http://localhost:3000`.

---

## 📖 Example Weekly Plan

| Day | Focus | Sample Exercises | Sample Meals |
|---|---|---|---|
| Monday | Upper Body | Bench Press, Rows, Shoulder Press | High-protein breakfast, chicken lunch |
| Tuesday | Cardio | Running, Cycling | Balanced macros |
| Wednesday | Lower Body | Squats, Deadlifts, Lunges | Recovery meals |
| Thursday | Rest | Light Stretching | Maintenance calories |
| Friday | Full Body | Compound Movements | Pre-weekend prep |
| Saturday | Functional | Kettlebell, Core | Flexible eating |
| Sunday | Active Recovery | Mobility work | Meal prep day |
