package project.FitAndFunGym.controller;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.dto.TrainingPlanDto.TrainingPlanResponseDto;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.service.TrainingPlanService;


@RestController
@RequestMapping(value = "/project")
public class TrainingPlanController {

    private final TrainingPlanService trainingPlanService;

    public TrainingPlanController(TrainingPlanService trainingPlanService) {
        this.trainingPlanService = trainingPlanService;
    }

    @GetMapping(value = "/trainingPlans")
    public ResponseEntity<Page<TrainingPlanResponseDto>> getAll(@RequestParam(name = "page", defaultValue = "0")  int page,
                                                                @RequestParam(name = "size", defaultValue = "5")  int size,
                                                                @RequestParam(name = "sortField", defaultValue = "name")  String sortField,
                                                                @RequestParam(name = "sortDirection", defaultValue = "ASC")  String sortDirection){
        return ResponseEntity.ok(trainingPlanService.getAll(page, size, sortField, sortDirection));
    }

    @GetMapping(value = "/trainingPlans/{id}")
    public ResponseEntity<TrainingPlan> getById(@PathVariable Long id) {
        return ResponseEntity.ok(trainingPlanService.getById(id));
    }

    @PostMapping(value = "/trainingPlans/add")
    public ResponseEntity<TrainingPlan> create(@RequestBody  TrainingPlan trainingPlan){
        return ResponseEntity.status(HttpStatus.CREATED).body(trainingPlanService.create(trainingPlan));
    }

}
