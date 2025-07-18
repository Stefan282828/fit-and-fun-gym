package project.FitAndFunGym.service;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.dto.ExerciseDto.ExerciseResponseDto;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.mapper.ExerciseMapper;
import project.FitAndFunGym.repository.ExerciseRepository;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.validator.ExerciseValidator;
import project.FitAndFunGym.util.ValidateUtil;

import java.util.List;


@Service
public class ExerciseService {

    private final ExerciseRepository exerciseRepository;
    private final TrainingPlanRepository trainingPlanRepository;
    private final ExerciseValidator exerciseValidator;

    public ExerciseService(ExerciseRepository exerciseRepository,
                           TrainingPlanRepository trainingPlanRepository,
                           ExerciseValidator exerciseValidator) {
        this.exerciseRepository = exerciseRepository;
        this.trainingPlanRepository = trainingPlanRepository;
        this.exerciseValidator = exerciseValidator;
    }

    @Transactional(readOnly = true)
    public Page<ExerciseResponseDto> getAll(int page, int size, String sortField, String sortDirection) {
        Sort sort = Sort.by(Sort.Direction.fromString(sortDirection), sortField);
        Pageable pageable = PageRequest.of(page, size, sort);
        return exerciseRepository.findAll(pageable).map(ExerciseMapper::toDto);
    }

    @Transactional(readOnly = true)
    public Exercise getById(Long id) {
        exerciseValidator.doesExist(id);
        return exerciseRepository.findById(id).get();
    }

    @Transactional
    public Exercise create(Exercise exercise) {
        exerciseValidator.validCreate(exercise);
        return exerciseRepository.save(exercise);
    }

    @Transactional
    public void delete(Long id) {
        exerciseValidator.doesExist(id);
        exerciseRepository.deleteById(id);
    }

//    @Transactional
//    public Exercise update(Exercise exercise) {
//        exerciseValidator.validUpdate(exercise);
//        Exercise existing = exerciseRepository.findById(exercise.getId()).get();
//
//        if (StringUtils.isNotBlank(exercise.getName())) {
//            existing.setName(exercise.getName());
//        }
//        if (StringUtils.isNotBlank(exercise.getDescription())) {
//            existing.setDescription(exercise.getDescription());
//        }
//        if (StringUtils.isNotBlank(exercise.getDifficulty())) {
//            existing.setDifficulty(exercise.getDifficulty());
//        }
//        if (exercise.getTrainingPlan() != null) {
//            existing.setTrainingPlan(exercise.getTrainingPlan());
//        }
//
//        return exerciseRepository.save(existing);
//    }

    @Transactional(readOnly = true)
    public List<ExerciseResponseDto> findByMuscleGroup(String muscleGroup){
        ValidateUtil.isValid(muscleGroup, "Muscle group");
        return exerciseRepository.findByMuscleGroupContaining(muscleGroup);
    }

    @Transactional(readOnly = true)
    public String getExerciseDescription(String exName){
        exerciseValidator.doesExist(exName);
        return exerciseRepository.getExerciseDescription(exName);
    }

}
