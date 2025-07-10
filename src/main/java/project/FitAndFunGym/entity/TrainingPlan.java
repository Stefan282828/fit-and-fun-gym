package project.FitAndFunGym.entity;

import jakarta.persistence.*;

import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name ="training_plan")
public class TrainingPlan {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "trainingPlan_seq")
    @SequenceGenerator(name = "trainingPlan_seq", sequenceName = "trainingPlan_id_seq", allocationSize = 1)
    @Column(name = "id")
    private Long id;
    @Column(name = "name")
    private String name;
    @Column(name = "goal")
    private String goal;
    @Column(name = "difficulty")
    private String difficulty;
    @Column(name = "duration")
    private String duration;
    @ManyToMany
    @JoinTable(
            name = "training_plan_exercise",
            joinColumns = @JoinColumn(name = "training_plan_id"),
            inverseJoinColumns = @JoinColumn (name = "exercise_id")
    )
    private Set<Exercise> exercises;

    @OneToMany(mappedBy = "trainingPlan", cascade = CascadeType.ALL)
    private Set<UserTrainingPlan> users = new HashSet<>();


    public Set<UserTrainingPlan> getUsers() {
        return users;
    }

    public void setUsers(Set<UserTrainingPlan> users) {
        this.users = users;
    }

    public Set<Exercise> getExercises() {
        return exercises;
    }

    public void setExercises(Set<Exercise> exercises) {
        this.exercises = exercises;
    }

    public TrainingPlan(){

    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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
}
