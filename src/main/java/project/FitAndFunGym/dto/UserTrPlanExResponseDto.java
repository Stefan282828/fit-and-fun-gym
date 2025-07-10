package project.FitAndFunGym.dto;

import java.util.List;

public class UserTrPlanExResponseDto {

    private Long userId;
    private String userName;
    private String userLastName;
    private String trainginPlanName;
    private List<String> exercises;

    public UserTrPlanExResponseDto(Long userId, String userName, String userLastName, String trainginPlanName, List<String> exercises) {
        this.userId = userId;
        this.userName = userName;
        this.userLastName = userLastName;
        this.trainginPlanName = trainginPlanName;
        this.exercises = exercises;
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

    public String getTrainginPlanName() {
        return trainginPlanName;
    }

    public void setTrainginPlanName(String trainginPlanName) {
        this.trainginPlanName = trainginPlanName;
    }

    public List<String> getExercises() {
        return exercises;
    }

    public void setExercises(List<String> exercises) {
        this.exercises = exercises;
    }
}
