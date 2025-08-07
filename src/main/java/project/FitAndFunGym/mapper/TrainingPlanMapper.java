package project.FitAndFunGym.mapper;

import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import project.FitAndFunGym.dto.TrainingPlanDto.TrainingPlanRequestDto;
import project.FitAndFunGym.dto.TrainingPlanDto.TrainingPlanResponseDto;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.exception.BadRequestException;

import java.util.List;
import java.util.Objects;

@Component
public class TrainingPlanMapper {

    public static TrainingPlan toEntity (TrainingPlanRequestDto trainingPlanRequestDto){
        if (Objects.isNull(trainingPlanRequestDto)){
            throw new BadRequestException("TrainingPlanRequestDto is null");
        }
        return new TrainingPlan(trainingPlanRequestDto.getId(),
                trainingPlanRequestDto.getName(),
                trainingPlanRequestDto.getGoal(),
                trainingPlanRequestDto.getDifficulty(),
                trainingPlanRequestDto.getDuration(),
                trainingPlanRequestDto.getDescription(),
                trainingPlanRequestDto.getCreatedByCoachId());
    }

    public static TrainingPlanResponseDto toDto (TrainingPlan trainingPlan){
        if (Objects.isNull(trainingPlan)){
            throw new BadRequestException("Training plan is null");
        }

        return new TrainingPlanResponseDto(trainingPlan.getName(),
                trainingPlan.getGoal(),
                trainingPlan.getDifficulty(),
                trainingPlan.getDuration(),
                trainingPlan.getDescription());
    }

    public static List<TrainingPlan> toEntityList (List<TrainingPlanRequestDto> trainingPlanRequestDtoList){
        if (ObjectUtils.isEmpty(trainingPlanRequestDtoList)){
            throw new BadRequestException("TrainingPlanRequestDto list is null");
        }
        return trainingPlanRequestDtoList.stream().map(TrainingPlanMapper::toEntity).toList();
    }

    public static List<TrainingPlanResponseDto> toDtoList (List<TrainingPlan> trainingPlanList){
        if (ObjectUtils.isEmpty(trainingPlanList)){
            throw new BadRequestException("Training plan list is null");
        }
        return trainingPlanList.stream().map(TrainingPlanMapper::toDto).toList();
    }

}
