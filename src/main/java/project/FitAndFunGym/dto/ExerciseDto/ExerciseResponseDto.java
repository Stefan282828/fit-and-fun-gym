package project.FitAndFunGym.dto.ExerciseDto;

import project.FitAndFunGym.entity.TrainingPlan;

import java.util.Set;

public class ExerciseResponseDto {

    private String name;
    private String description;
    private String muscleGroup;

    public ExerciseResponseDto(String name, String description, String muscleGroup) {
        this.name = name;
        this.description = description;
        this.muscleGroup = muscleGroup;
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
}
