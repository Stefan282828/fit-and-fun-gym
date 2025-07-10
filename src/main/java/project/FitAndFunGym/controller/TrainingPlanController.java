package project.FitAndFunGym.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.dto.UserDto.UserResponseDto;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.entity.TrainingPlan;
import project.FitAndFunGym.service.TrainingPlanService;

import java.util.List;
import java.util.Set;

@RestController
@RequestMapping(value = "/project")
public class TrainingPlanController {

    private final TrainingPlanService trainingPlanService;

    public TrainingPlanController(TrainingPlanService trainingPlanService) {
        this.trainingPlanService = trainingPlanService;
    }

    @GetMapping
    public ResponseEntity<List<TrainingPlan>> getAll() {
        return ResponseEntity.ok(trainingPlanService.getAll());
    }

    @GetMapping(value = "/trainingPlans/{id}")
    public ResponseEntity<TrainingPlan> getById(@PathVariable Long id) {
        return ResponseEntity.ok(trainingPlanService.getById(id));
    }

    @PostMapping(value = "/trainingPlans/add")
    public ResponseEntity<TrainingPlan> add(@RequestBody  TrainingPlan trainingPlan){
        return ResponseEntity.ok(trainingPlanService.create(trainingPlan));
    }

    @PreAuthorize("hasRole('COACH')")
    @PostMapping(value = "/trainingPlans/coach/addExercises")
    public ResponseEntity<String> addExercises(@RequestBody Set<Exercise> exercises, @RequestParam Long trainingPlanId){
        trainingPlanService.addExercises(exercises,trainingPlanId);
        return ResponseEntity.ok("Added successfully");
    }
}
