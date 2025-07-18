package project.FitAndFunGym.dto.TrainingPlanDto;

import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.entity.UserTrainingPlan;

import java.util.Set;

public class TrainingPlanRequestDto {

    private Long id;
    private String name;
    private String goal;
    private String difficulty;
    private String duration;
    private Set<Exercise> exercises;
    private Set<UserTrainingPlan> users;

    public TrainingPlanRequestDto(Long id, String name, String goal, String difficulty, String duration, Set<Exercise> exercises, Set<UserTrainingPlan> users) {
        this.id = id;
        this.name = name;
        this.goal = goal;
        this.difficulty = difficulty;
        this.duration = duration;
        this.exercises = exercises;
        this.users = users;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public Set<Exercise> getExercises() {
        return exercises;
    }

    public void setExercises(Set<Exercise> exercises) {
        this.exercises = exercises;
    }

    public Set<UserTrainingPlan> getUsers() {
        return users;
    }

    public void setUsers(Set<UserTrainingPlan> users) {
        this.users = users;
    }
}
