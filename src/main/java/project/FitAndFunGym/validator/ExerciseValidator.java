package project.FitAndFunGym.validator;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.ExerciseRepository;

import java.util.Collection;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class ExerciseValidator {

    public final ExerciseRepository exerciseRepository;

    public ExerciseValidator(ExerciseRepository exerciseRepository) {
        this.exerciseRepository = exerciseRepository;
    }

    public void doesExistById(Long id){
        IsValidValidator.isValidId(id);
        if(Boolean.FALSE.equals(exerciseRepository.existsById(id))){
            throw new BadRequestException(String.format("Exercise with id %s not found", id));
        }
    }

    public void alreadyExists(Exercise exercise){
        isValidExercise(exercise);
        if(Boolean.TRUE.equals(exerciseRepository.existsByName(exercise.getName()))){
            throw new BadRequestException(String.format("Exercise with name %s already exists", exercise.getName()));
        }
    }

    public void isValidExercise(Exercise exercise){
        if(Objects.isNull(exercise)){
            throw new BadRequestException("Exercise cannot be null");
        }
    }

    public void validCreate(Exercise exercise){
        StringValidator.validateString(exercise.getName(), "name");
        StringValidator.validateString(exercise.getDescription(), "description");
        StringValidator.validateString(exercise.getMuscleGroup(), "muscle group");
        alreadyExists(exercise);
    }

    public void areValidExercises(Collection<Exercise> exercises){
        if(CollectionUtils.isEmpty(exercises)){
            throw new BadRequestException("Exercises cannot be empty");
        }
        Collection<Exercise> ex = exercises.stream()
                .map(exercise -> exerciseRepository.findById(exercise.getId()).orElseThrow(()->new BadRequestException(String.format("Exercise with id %s does not exist", exercise.getId()))))
                .toList();
    }
}
