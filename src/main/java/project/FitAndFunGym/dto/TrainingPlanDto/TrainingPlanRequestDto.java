package project.FitAndFunGym.dto.TrainingPlanDto;


public class TrainingPlanRequestDto {

    private Long id;
    private String name;
    private String goal;
    private String difficulty;
    private String duration;
    private String description;
    private Long createdByCoachId;

    public TrainingPlanRequestDto(Long id, String name, String goal, String difficulty, String duration, String description, Long createdByCoachId) {
        this.id = id;
        this.name = name;
        this.goal = goal;
        this.difficulty = difficulty;
        this.duration = duration;
        this.description = description;
        this.createdByCoachId = createdByCoachId;
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
