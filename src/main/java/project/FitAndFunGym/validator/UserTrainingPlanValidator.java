package project.FitAndFunGym.validator;

import org.springframework.stereotype.Service;
import project.FitAndFunGym.entity.Status;
import project.FitAndFunGym.entity.UserTrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.UserTrainingPlanRepository;

import java.util.Optional;

@Service
public class UserTrainingPlanValidator {

    private final UserTrainingPlanRepository userTrainingPlanRepository;
    private final UserValidator userValidator;

    public UserTrainingPlanValidator(UserTrainingPlanRepository userTrainingPlanRepository, UserValidator userValidator) {
        this.userTrainingPlanRepository = userTrainingPlanRepository;
        this.userValidator = userValidator;
    }

    public void doesContainsTrPlan(Long userId){
        userValidator.doesExistById(userId);
        if(Boolean.FALSE.equals(userTrainingPlanRepository.existsByUser_Id(userId))){
            throw new BadRequestException(String.format("User with id %s does not contain training plan", userId));
        }
    }

    public void hasActivePlan(Long id){
        Optional<UserTrainingPlan> userActivePlan = userTrainingPlanRepository.findByUser_IdAndStatus(id, Status.ACTIVE);
        if(userActivePlan.isPresent()){
            throw new BadRequestException(String.format("User with id %s already has an active training plan", id));
        }
    }
}
