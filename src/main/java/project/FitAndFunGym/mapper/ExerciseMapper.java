package project.FitAndFunGym.mapper;

import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import project.FitAndFunGym.dto.ExerciseDto.ExerciseRequestDto;
import project.FitAndFunGym.dto.ExerciseDto.ExerciseResponseDto;
import project.FitAndFunGym.dto.UserDto.UserRequestDto;
import project.FitAndFunGym.dto.UserDto.UserResponseDto;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.User;
import project.FitAndFunGym.exception.BadRequestException;

import java.util.List;
import java.util.Objects;

@Component
public class ExerciseMapper {

    public static Exercise toEntity (ExerciseRequestDto exerciseRequestDto){
        if (Objects.isNull(exerciseRequestDto)){
            throw new BadRequestException("ExerciseRequestDto is null");
        }
        return new Exercise(exerciseRequestDto.getId(),
                exerciseRequestDto.getName(),
                exerciseRequestDto.getDescription(),
                exerciseRequestDto.getMuscleGroup(),
                exerciseRequestDto.getTrainingPlans());
    }

    public static ExerciseResponseDto toDto (Exercise exercise){
        if (Objects.isNull(exercise)){
            throw new BadRequestException("Exercise is null");
        }
        return new ExerciseResponseDto(exercise.getName(),
                exercise.getDescription(),
                exercise.getMuscleGroup());
    }

    public static List<Exercise> toEntityList (List<ExerciseRequestDto> exerciseRequestDtoList){
        if (ObjectUtils.isEmpty(exerciseRequestDtoList)){
            throw new BadRequestException("ExerciseRequestDto list is null");
        }
        return exerciseRequestDtoList.stream().map(ExerciseMapper::toEntity).toList();
    }

    public static List<ExerciseResponseDto> toDtoList (List<Exercise> exerciseList){
        if (ObjectUtils.isEmpty(exerciseList)){
            throw new BadRequestException("Exercise list is null");
        }
        return exerciseList.stream().map(ExerciseMapper::toDto).toList();
    }
}
