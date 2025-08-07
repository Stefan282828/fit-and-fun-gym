package project.FitAndFunGym.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.dto.TrainingPlanDto.TrainingPlanResponseDto;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.mapper.TrainingPlanMapper;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.validator.TrainingPlanValidator;



@Service
public class TrainingPlanService {

    TrainingPlanRepository trainingPlanRepository;
    TrainingPlanValidator trainingPlanValidator;

    public TrainingPlanService(TrainingPlanRepository trainingPlanRepository, TrainingPlanValidator trainingPlanValidator) {
        this.trainingPlanRepository = trainingPlanRepository;
        this.trainingPlanValidator = trainingPlanValidator;
    }

    @Transactional(readOnly = true)
    public Page<TrainingPlanResponseDto> getAll(int page, int size, String sortField, String sortDirection) {
        Sort sort = Sort.by(Sort.Direction.fromString(sortDirection), sortField);
        Pageable pageable = PageRequest.of(page, size, sort);
        return trainingPlanRepository.findAll(pageable).map(TrainingPlanMapper::toDto);
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


}
