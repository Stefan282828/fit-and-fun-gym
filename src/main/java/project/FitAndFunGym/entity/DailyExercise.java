package project.FitAndFunGym.entity;

import jakarta.persistence.*;

@Entity
@Table(name = "daily_exercise")
public class DailyExercise {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "daily_exercise_seq")
    @SequenceGenerator(name = "daily_exercise_seq", sequenceName = "daily_exercise_id_seq", allocationSize = 1)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "daily_plan_id", nullable = false)
    private DailyPlan dailyPlan;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "exercise_id", nullable = false)
    private Exercise exercise;

    @Column(name = "sets")
    private Integer sets;

    @Column(name = "reps")
    private String reps;

    @Column(name = "weight")
    private String weight;

    @Column(name = "rest_time")
    private String restTime;

    @Column(name = "order_in_workout")
    private Integer orderInWorkout;

    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    public DailyExercise() {}

    public DailyExercise(DailyPlan dailyPlan, Exercise exercise, Integer sets, String reps, String weight, Integer orderInWorkout) {
        this.dailyPlan = dailyPlan;
        this.exercise = exercise;
        this.sets = sets;
        this.reps = reps;
        this.weight = weight;
        this.orderInWorkout = orderInWorkout;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public DailyPlan getDailyPlan() {
        return dailyPlan;
    }

    public void setDailyPlan(DailyPlan dailyPlan) {
        this.dailyPlan = dailyPlan;
    }

    public Exercise getExercise() {
        return exercise;
    }

    public void setExercise(Exercise exercise) {
        this.exercise = exercise;
    }

    public Integer getSets() {
        return sets;
    }

    public void setSets(Integer sets) {
        this.sets = sets;
    }

    public String getReps() {
        return reps;
    }

    public void setReps(String reps) {
        this.reps = reps;
    }

    public String getWeight() {
        return weight;
    }

    public void setWeight(String weight) {
        this.weight = weight;
    }

    public String getRestTime() {
        return restTime;
    }

    public void setRestTime(String restTime) {
        this.restTime = restTime;
    }

    public Integer getOrderInWorkout() {
        return orderInWorkout;
    }

    public void setOrderInWorkout(Integer orderInWorkout) {
        this.orderInWorkout = orderInWorkout;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}
