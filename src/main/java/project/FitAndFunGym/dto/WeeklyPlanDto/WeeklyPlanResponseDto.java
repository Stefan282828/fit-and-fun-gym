package project.FitAndFunGym.dto.WeeklyPlanDto;

import project.FitAndFunGym.entity.DayOfWeek;

import java.util.List;
import java.util.Map;

public class WeeklyPlanResponseDto {

    private Long trainingPlanId;
    private String trainingPlanName;
    private String goal;
    private String difficulty;
    private String duration;
    private String description;
    private Map<DayOfWeek, DailyPlanResponseDto> dailyPlans;

    public WeeklyPlanResponseDto() {}

    public WeeklyPlanResponseDto(Long trainingPlanId, String trainingPlanName, String goal, 
                                String difficulty, String duration, String description,
                                Map<DayOfWeek, DailyPlanResponseDto> dailyPlans) {
        this.trainingPlanId = trainingPlanId;
        this.trainingPlanName = trainingPlanName;
        this.goal = goal;
        this.difficulty = difficulty;
        this.duration = duration;
        this.description = description;
        this.dailyPlans = dailyPlans;
    }

    // Getters and Setters
    public Long getTrainingPlanId() {
        return trainingPlanId;
    }

    public void setTrainingPlanId(Long trainingPlanId) {
        this.trainingPlanId = trainingPlanId;
    }

    public String getTrainingPlanName() {
        return trainingPlanName;
    }

    public void setTrainingPlanName(String trainingPlanName) {
        this.trainingPlanName = trainingPlanName;
    }

    public String getGoal() {
        return goal;
    }

    public void setGoal(String goal) {
        this.goal = goal;
    }

    public String getDifficulty() {
        return difficulty;
    }

    public void setDifficulty(String difficulty) {
        this.difficulty = difficulty;
    }

    public String getDuration() {
        return duration;
    }

    public void setDuration(String duration) {
        this.duration = duration;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Map<DayOfWeek, DailyPlanResponseDto> getDailyPlans() {
        return dailyPlans;
    }

    public void setDailyPlans(Map<DayOfWeek, DailyPlanResponseDto> dailyPlans) {
        this.dailyPlans = dailyPlans;
    }
}