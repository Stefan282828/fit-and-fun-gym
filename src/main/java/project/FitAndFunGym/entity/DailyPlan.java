package project.FitAndFunGym.entity;

import jakarta.persistence.*;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "daily_plan")
public class DailyPlan {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "daily_plan_seq")
    @SequenceGenerator(name = "daily_plan_seq", sequenceName = "daily_plan_id_seq", allocationSize = 1)
    private Long id;

    @Enumerated(EnumType.STRING)
    @Column(name = "day_of_week", nullable = false)
    private DayOfWeek dayOfWeek;

    @Column(name = "focus_area")
    private String focusArea; // e.g., "Upper Body", "Cardio", "Rest Day", "Full Body"

    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes; // Special instructions for the day

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "training_plan_id")
    private TrainingPlan trainingPlan;

    @OneToMany(mappedBy = "dailyPlan", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<DailyExercise> exercises = new HashSet<>();

    @OneToMany(mappedBy = "dailyPlan", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<DailyMeal> meals = new HashSet<>();

    public DailyPlan() {}

    public DailyPlan(DayOfWeek dayOfWeek, String focusArea, TrainingPlan trainingPlan) {
        this.dayOfWeek = dayOfWeek;
        this.focusArea = focusArea;
        this.trainingPlan = trainingPlan;
    }

    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public DayOfWeek getDayOfWeek() {
        return dayOfWeek;
    }

    public void setDayOfWeek(DayOfWeek dayOfWeek) {
        this.dayOfWeek = dayOfWeek;
    }

    public String getFocusArea() {
        return focusArea;
    }

    public void setFocusArea(String focusArea) {
        this.focusArea = focusArea;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public TrainingPlan getTrainingPlan() {
        return trainingPlan;
    }

    public void setTrainingPlan(TrainingPlan trainingPlan) {
        this.trainingPlan = trainingPlan;
    }

    public Set<DailyExercise> getExercises() {
        return exercises;
    }

    public void setExercises(Set<DailyExercise> exercises) {
        this.exercises = exercises;
    }

    public Set<DailyMeal> getMeals() {
        return meals;
    }

    public void setMeals(Set<DailyMeal> meals) {
        this.meals = meals;
    }
}