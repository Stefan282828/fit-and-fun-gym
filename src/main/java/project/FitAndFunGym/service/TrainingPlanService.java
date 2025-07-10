package project.FitAndFunGym.service;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.ExerciseRepository;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.validator.ExerciseValidator;
import project.FitAndFunGym.validator.IsValidValidator;
import project.FitAndFunGym.validator.TrainingPlanValidator;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


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
        trainingPlanValidator.doesExistById(id);
        return trainingPlanRepository.findById(id).get();
    }

    @Transactional
    public TrainingPlan create(TrainingPlan trainingPlan){
        trainingPlanValidator.validCreate(trainingPlan);
        return trainingPlanRepository.save(trainingPlan);
    }


    @Transactional
    public void addExercises(Set<Exercise> exercises, Long id){
        IsValidValidator.isValidId(id);
        TrainingPlan trainingPlan =trainingPlanRepository.findById(id).orElseThrow(()-> new BadRequestException("Training plan with that id not found"));
        exerciseValidator.areValidExercises(exercises);
        trainingPlan.getExercises().addAll(exercises);
        trainingPlanRepository.save(trainingPlan);
    }

}
