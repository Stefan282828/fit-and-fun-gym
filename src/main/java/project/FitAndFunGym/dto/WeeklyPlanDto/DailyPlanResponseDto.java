package project.FitAndFunGym.dto.WeeklyPlanDto;

import project.FitAndFunGym.entity.DayOfWeek;
import project.FitAndFunGym.entity.MealType;

import java.util.List;
import java.util.Map;

public class DailyPlanResponseDto {

    private Long id;
    private DayOfWeek dayOfWeek;
    private String focusArea;
    private String notes;
    private List<DailyExerciseResponseDto> exercises;
    private Map<MealType, List<DailyMealResponseDto>> meals;

    public DailyPlanResponseDto() {}

    public DailyPlanResponseDto(Long id, DayOfWeek dayOfWeek, String focusArea, String notes,
                               List<DailyExerciseResponseDto> exercises,
                               Map<MealType, List<DailyMealResponseDto>> meals) {
        this.id = id;
        this.dayOfWeek = dayOfWeek;
        this.focusArea = focusArea;
        this.notes = notes;
        this.exercises = exercises;
        this.meals = meals;
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

    public List<DailyExerciseResponseDto> getExercises() {
        return exercises;
    }

    public void setExercises(List<DailyExerciseResponseDto> exercises) {
        this.exercises = exercises;
    }

    public Map<MealType, List<DailyMealResponseDto>> getMeals() {
        return meals;
    }

    public void setMeals(Map<MealType, List<DailyMealResponseDto>> meals) {
        this.meals = meals;
    }
}