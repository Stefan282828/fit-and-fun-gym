package project.FitAndFunGym.entity;

import jakarta.persistence.*;

import java.util.Set;

@Entity
@Table(name = "exercise")
public class Exercise {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "exercise_seq")
    @SequenceGenerator(name = "exercise_seq", sequenceName = "exercise_id_seq", allocationSize = 1)
    private Long id;
    @Column(name = "name")
    private String name;
    @Column(name = "description")
    private String description;
    @Column(name = "muscleGroup")
    private String muscleGroup;
    @ManyToMany(mappedBy = "exercises")
    private Set<TrainingPlan> trainingPlans;

    public Exercise(Long id, String name, String description, String muscleGroup, Set<TrainingPlan> trainingPlans) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.muscleGroup = muscleGroup;
        this.trainingPlans = trainingPlans;
    }

    public Exercise(){
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
