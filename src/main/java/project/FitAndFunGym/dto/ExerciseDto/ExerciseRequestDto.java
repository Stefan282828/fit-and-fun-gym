package project.FitAndFunGym.dto.ExerciseDto;

import project.FitAndFunGym.entity.TrainingPlan;

import java.util.Set;

public class ExerciseRequestDto {

    private Long id;
    private String name;
    private String description;
    private String muscleGroup;
    private Set<TrainingPlan> trainingPlans;

    public ExerciseRequestDto(Long id, String name, String description, String muscleGroup, Set<TrainingPlan> trainingPlans) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.muscleGroup = muscleGroup;
        this.trainingPlans = trainingPlans;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getMuscleGroup() {
        return muscleGroup;
    }

    public void setMuscleGroup(String muscleGroup) {
        this.muscleGroup = muscleGroup;
    }

    public Set<TrainingPlan> getTrainingPlans() {
        return trainingPlans;
    }

    public void setTrainingPlans(Set<TrainingPlan> trainingPlans) {
        this.trainingPlans = trainingPlans;
    }
}
