package project.FitAndFunGym.entity;

import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

@Embeddable
public class UserTrainingPlanId implements Serializable {

    private Long userId;
    private Long trainingPlanId;

    public UserTrainingPlanId(){}

    public UserTrainingPlanId(Long userId, Long trainingPlanId) {
        this.userId = userId;
        this.trainingPlanId = trainingPlanId;
    }

    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(!(o instanceof UserTrainingPlanId)) return false;
        UserTrainingPlanId that = (UserTrainingPlanId) o;
        return Objects.equals(userId, that.userId) &&
                Objects.equals(trainingPlanId, that.trainingPlanId);
    }

    @Override
    public int hashCode(){
        return Objects.hash(userId, trainingPlanId);
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public Long getTrainingPlanId() {
        return trainingPlanId;
    }

    public void setTrainingPlanId(Long trainingPlanId) {
        this.trainingPlanId = trainingPlanId;
    }
}
