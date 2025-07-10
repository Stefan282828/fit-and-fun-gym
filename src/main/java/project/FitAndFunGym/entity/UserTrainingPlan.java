package project.FitAndFunGym.entity;

import jakarta.persistence.*;

import java.time.LocalDate;

@Entity
@Table(name = "user_training_plan")
public class UserTrainingPlan {

    @EmbeddedId
    private  UserTrainingPlanId id = new UserTrainingPlanId();

    @ManyToOne
    @MapsId("userId")
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne
    @MapsId("trainingPlanId")
    @JoinColumn(name = "training_plan_id")
    private TrainingPlan trainingPlan;

    @Column(name = "assigned_date")
    private LocalDate assignedDate = LocalDate.now();

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private Status status = Status.ACTIVE;

    public UserTrainingPlan(User user, TrainingPlan trainingPlan) {
        this.id = new UserTrainingPlanId(user.getId(), trainingPlan.getId());
        this.user = user;
        this.trainingPlan = trainingPlan;
        this.assignedDate = LocalDate.now();
        this.status = Status.ACTIVE;
    }

    public UserTrainingPlan() {}

    public UserTrainingPlanId getId() {
        return id;
    }

    public void setId(UserTrainingPlanId id) {
        this.id = id;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public TrainingPlan getTrainingPlan() {
        return trainingPlan;
    }

    public void setTrainingPlan(TrainingPlan trainingPlan) {
        this.trainingPlan = trainingPlan;
    }

    public LocalDate getAssignedDate() {
        return assignedDate;
    }

    public void setAssignedDate(LocalDate assignedDate) {
        this.assignedDate = assignedDate;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }
}
