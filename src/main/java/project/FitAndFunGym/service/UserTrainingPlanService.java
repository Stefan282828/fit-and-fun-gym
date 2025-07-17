package project.FitAndFunGym.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.dto.UserTrPlanExRequestDto;
import project.FitAndFunGym.dto.UserTrPlanExResponseDto;
import project.FitAndFunGym.entity.UserTrainingPlanId;
import project.FitAndFunGym.repository.UserTrainingPlanRepository;
import project.FitAndFunGym.validator.TrainingPlanValidator;
import project.FitAndFunGym.validator.UserTrainingPlanValidator;
import project.FitAndFunGym.validator.UserValidator;

import java.util.List;

@Service
public class UserTrainingPlanService {

    private final UserTrainingPlanRepository userTrainingPlanRepository;
    private final UserTrainingPlanValidator userTrainingPlanValidator;
    private final UserValidator userValidator;
    private final TrainingPlanValidator trainingPlanValidator;

    public UserTrainingPlanService(UserTrainingPlanRepository userTrainingPlanRepository, UserTrainingPlanValidator userTrainingPlanValidator, UserValidator userValidator, TrainingPlanValidator trainingPlanValidator) {
        this.userTrainingPlanRepository = userTrainingPlanRepository;
        this.userTrainingPlanValidator = userTrainingPlanValidator;
        this.userValidator = userValidator;
        this.trainingPlanValidator = trainingPlanValidator;
    }

    @Transactional
    public void delete (Long userId, Long trainingPlanId){
         userValidator.doesExist(userId);
         trainingPlanValidator.doesExist(trainingPlanId);
         UserTrainingPlanId userTrainingPlanId = new UserTrainingPlanId(userId, trainingPlanId);
         userTrainingPlanRepository.deleteById(userTrainingPlanId);
    }

    @Transactional(readOnly = true)
    public UserTrPlanExResponseDto getTrPlanExercises (Long userId){
        userTrainingPlanValidator.doesContainsTrPlan(userId);
        List<UserTrPlanExRequestDto> rows = userTrainingPlanRepository.findTrPlanExercisesByUserId(userId);
        List<String> exercises = rows.stream()
                .map(UserTrPlanExRequestDto::getExerciseName)
                .toList();
        UserTrPlanExRequestDto first = rows.get(0);

        return new UserTrPlanExResponseDto(
                first.getUserId(),
                first.getUserName(),
                first.getUserLastName(),
                first.getTrainingPlanName(),
                exercises
        );

    }
}
