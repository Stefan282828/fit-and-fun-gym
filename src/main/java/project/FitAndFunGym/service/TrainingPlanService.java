package project.FitAndFunGym.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.ExerciseRepository;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.validator.ExerciseValidator;
import project.FitAndFunGym.util.ValidateUtil;
import project.FitAndFunGym.validator.TrainingPlanValidator;

import java.util.*;


@Service
public class TrainingPlanService {

    TrainingPlanRepository trainingPlanRepository;
    TrainingPlanValidator trainingPlanValidator;
    ExerciseRepository exerciseRepository;
    ExerciseValidator exerciseValidator;

    public TrainingPlanService(TrainingPlanRepository trainingPlanRepository, TrainingPlanValidator trainingPlanValidator, ExerciseRepository exerciseRepository, ExerciseValidator exerciseValidator) {
        this.trainingPlanRepository = trainingPlanRepository;
        this.trainingPlanValidator = trainingPlanValidator;
        this.exerciseRepository = exerciseRepository;
        this.exerciseValidator = exerciseValidator;
    }

    @Transactional(readOnly = true)
    public List<TrainingPlan> getAll(){
        return trainingPlanRepository.findAll();
    }

    @Transactional(readOnly = true)
    public TrainingPlan getById(Long id){
        trainingPlanValidator.doesExist(id);
        return trainingPlanRepository.findById(id).get();
    }

    @Transactional
    public TrainingPlan create(TrainingPlan trainingPlan){
        trainingPlanValidator.validCreate(trainingPlan);
        return trainingPlanRepository.save(trainingPlan);
    }


    @Transactional
    public void addExercises(Set<Exercise> exercises, Long id){
        ValidateUtil.isValid(id);
        TrainingPlan trainingPlan =trainingPlanRepository.findById(id).orElseThrow(()-> new BadRequestException("Training plan with that id not found"));
        exerciseValidator.validExercises(exercises);
        trainingPlan.getExercises().addAll(exercises);
        trainingPlanRepository.save(trainingPlan);
    }

    @Transactional(readOnly = true)
    public List<String> getExercisesForTrPlan(String trainingPlanName){
        ValidateUtil.isValid(trainingPlanName, "Name of the training plan");
        return trainingPlanRepository.findExercisesByTrPlanName(trainingPlanName);
    }

}
