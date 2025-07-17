package project.FitAndFunGym.validator;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.ExerciseRepository;
import project.FitAndFunGym.util.ValidateUtil;

import java.util.Collection;
import java.util.Objects;

@Service
public class ExerciseValidator {

    public final ExerciseRepository exerciseRepository;

    public ExerciseValidator(ExerciseRepository exerciseRepository) {
        this.exerciseRepository = exerciseRepository;
    }

    public void doesExist(Long id){
        ValidateUtil.isValid(id);
        if(Boolean.FALSE.equals(exerciseRepository.existsById(id))){
            throw new BadRequestException(String.format("Exercise with id %s not found", id));
        }
    }

    public void doesExist(String name){
        ValidateUtil.isValid(name, "Exercise name");
        if(Boolean.FALSE.equals(exerciseRepository.existsByName(name))){
            throw new BadRequestException(String.format("Exercise with name %s not found", name));
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
        ValidateUtil.isValid(exercise.getName(), "name");
        ValidateUtil.isValid(exercise.getDescription(), "description");
        ValidateUtil.isValid(exercise.getMuscleGroup(), "muscle group");
        alreadyExists(exercise);
    }

    public void validExercises(Collection<Exercise> exercises){
        if(CollectionUtils.isEmpty(exercises)){
            throw new BadRequestException("Exercises cannot be empty");
        }
        Collection<Exercise> ex = exercises.stream()
                .map(exercise -> exerciseRepository.findById(exercise.getId()).orElseThrow(()->new BadRequestException(String.format("Exercise with id %s does not exist", exercise.getId()))))
                .toList();
    }
}
