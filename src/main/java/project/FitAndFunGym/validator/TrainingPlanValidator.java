package project.FitAndFunGym.validator;

import org.springframework.stereotype.Service;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.util.ValidateUtil;

import java.util.Objects;

@Service
public class TrainingPlanValidator {

    TrainingPlanRepository trainingPlanRepository;

    public TrainingPlanValidator(TrainingPlanRepository trainingPlanRepository) {
        this.trainingPlanRepository = trainingPlanRepository;
    }

    public void doesExist(Long id){
        ValidateUtil.isValid(id);
        if(Boolean.FALSE.equals(trainingPlanRepository.existsById(id))){
            throw new BadRequestException(String.format("Training plan with id %s not found", id));
        }
    }

    public void alreadyExists(TrainingPlan trainingPlan){
        isValidTrainingPlan(trainingPlan);
        if(Boolean.TRUE.equals(trainingPlanRepository.existsByName(trainingPlan.getName()))){
            throw new BadRequestException(String.format("Training plan with name %s already exists", trainingPlan.getName()));
        }
    }

    public void isValidTrainingPlan(TrainingPlan trainingPlan){
        if(Objects.isNull(trainingPlan)){
            throw new BadRequestException("Training plan cannot be null");
        }
    }

    public void validCreate(TrainingPlan trainingPlan){
        ValidateUtil.isValid(trainingPlan.getName(), "name");
        ValidateUtil.isValid(trainingPlan.getGoal(), "goal");
        ValidateUtil.isValid(trainingPlan.getDifficulty(), "difficulty");
        ValidateUtil.isValid(trainingPlan.getDuration(), "duration");
        alreadyExists(trainingPlan);
    }
}
