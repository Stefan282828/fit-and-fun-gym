package project.FitAndFunGym.dto;

public class UserTrPlanExRequestDto {

    private Long userId;
    private String userName;
    private String userLastName;
    private String trainingPlanName;
    private String exerciseName;

    public UserTrPlanExRequestDto(Long userId, String userName, String userLastName, String trainingPlanName, String exerciseName) {
        this.userId = userId;
        this.userName = userName;
        this.userLastName = userLastName;
        this.trainingPlanName = trainingPlanName;
        this.exerciseName = exerciseName;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserLastName() {
        return userLastName;
    }

    public void setUserLastName(String userLastName) {
        this.userLastName = userLastName;
    }

    public String getTrainingPlanName() {
        return trainingPlanName;
    }

    public void setTrainingPlanName(String trainingPlanName) {
        this.trainingPlanName = trainingPlanName;
    }

    public String getExerciseName() {
        return exerciseName;
    }

    public void setExerciseName(String exerciseName) {
        this.exerciseName = exerciseName;
    }

    @Override
    public String toString() {
        return "UserTrPlanExRequestDto{" +
                "userId=" + userId +
                ", userName='" + userName + '\'' +
                ", userLastName='" + userLastName + '\'' +
                ", trainingPlanName='" + trainingPlanName + '\'' +
                ", exerciseName='" + exerciseName + '\'' +
                '}';
    }

}


