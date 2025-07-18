package project.FitAndFunGym.dto.TrainingPlanDto;

import project.FitAndFunGym.entity.Exercise;

import java.util.Set;

public class TrainingPlanResponseDto {

    private String name;
    private String goal;
    private String difficulty;
    private String duration;
    private Set<String> exercisesName;

    public TrainingPlanResponseDto(String name, String goal, String difficulty, String duration, Set<String> exercisesName) {
        this.name = name;
        this.goal = goal;
        this.difficulty = difficulty;
        this.duration = duration;
        this.exercisesName = exercisesName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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

    public Set<String> getExercisesName() {
        return exercisesName;
    }

    public void setExercisesName(Set<String> exercisesName) {
        this.exercisesName = exercisesName;
    }
}
