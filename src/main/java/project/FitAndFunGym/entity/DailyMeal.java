package project.FitAndFunGym.entity;

import jakarta.persistence.*;

@Entity
@Table(name = "daily_meal")
public class DailyMeal {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "daily_meal_seq")
    @SequenceGenerator(name = "daily_meal_seq", sequenceName = "daily_meal_id_seq", allocationSize = 1)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "daily_plan_id", nullable = false)
    private DailyPlan dailyPlan;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "food_id", nullable = false)
    private Food food;

    @Enumerated(EnumType.STRING)
    @Column(name = "meal_type", nullable = false)
    private MealType mealType;

    @Column(name = "quantity")
    private Double quantity;

    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    public DailyMeal() {}

    public DailyMeal(DailyPlan dailyPlan, Food food, MealType mealType, Double quantity) {
        this.dailyPlan = dailyPlan;
        this.food = food;
        this.mealType = mealType;
        this.quantity = quantity;
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

    public Food getFood() {
        return food;
    }

    public void setFood(Food food) {
        this.food = food;
    }

    public MealType getMealType() {
        return mealType;
    }

    public void setMealType(MealType mealType) {
        this.mealType = mealType;
    }

    public Double getQuantity() {
        return quantity;
    }

    public void setQuantity(Double quantity) {
        this.quantity = quantity;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
}
