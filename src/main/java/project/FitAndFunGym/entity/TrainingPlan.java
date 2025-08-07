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
    
    @Column(name = "description", columnDefinition = "TEXT")
    private String description;
    
    @Column(name = "created_by_coach_id")
    private Long createdByCoachId; // Reference to the coach who created this plan

    @OneToMany(mappedBy = "trainingPlan", cascade = CascadeType.ALL)
    private Set<UserTrainingPlan> users = new HashSet<>();

    @OneToMany(mappedBy = "trainingPlan", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<DailyPlan> dailyPlans = new HashSet<>();

    public TrainingPlan(Long id, String name, String goal, String difficulty, String duration, String description, Long createdByCoachId) {
        this.id = id;
        this.name = name;
        this.goal = goal;
        this.difficulty = difficulty;
        this.duration = duration;
        this.description = description;
        this.createdByCoachId = createdByCoachId;
    }

    public Set<UserTrainingPlan> getUsers() {
        return users;
    }

    public void setUsers(Set<UserTrainingPlan> users) {
        this.users = users;
    }

    public Set<DailyPlan> getDailyPlans() {
        return dailyPlans;
    }

    public void setDailyPlans(Set<DailyPlan> dailyPlans) {
        this.dailyPlans = dailyPlans;
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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getCreatedByCoachId() {
        return createdByCoachId;
    }

    public void setCreatedByCoachId(Long createdByCoachId) {
        this.createdByCoachId = createdByCoachId;
    }
}
