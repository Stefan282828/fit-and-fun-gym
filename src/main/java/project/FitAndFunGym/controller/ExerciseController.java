package project.FitAndFunGym.controller;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.dto.ExerciseDto.ExerciseResponseDto;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.service.ExerciseService;

import java.util.List;

@RestController
@RequestMapping("/project")
public class ExerciseController {

    private final ExerciseService exerciseService;

    public ExerciseController(ExerciseService exerciseService) {
        this.exerciseService = exerciseService;
    }

    @GetMapping("/exercises")
    public ResponseEntity<Page<ExerciseResponseDto>> getAll(@RequestParam(name = "page", defaultValue = "0")  int page,
                                                            @RequestParam(name = "size", defaultValue = "5")  int size,
                                                            @RequestParam(name = "sortField", defaultValue = "name")  String sortField,
                                                            @RequestParam(name = "sortDirection", defaultValue = "ASC")  String sortDirection){
        return ResponseEntity.ok(exerciseService.getAll(page, size, sortField, sortDirection));
    }

    @GetMapping("/exercises/{id}")
    public ResponseEntity<Exercise> getById(@PathVariable Long id) {
        return ResponseEntity.ok(exerciseService.getById(id));
    }

    @PostMapping("/exercises/add")
    public ResponseEntity<Exercise> create(@RequestBody Exercise exercise) {
        return ResponseEntity.status(HttpStatus.CREATED).body(exerciseService.create(exercise));
    }

//    @PutMapping("/update")
//    public ResponseEntity<Exercise> update(@RequestBody Exercise exercise) {
//        return ResponseEntity.ok(exerciseService.update(exercise));
//    }

    @DeleteMapping("/exercises/delete")
    public ResponseEntity<String> delete(@RequestParam Long id) {
        exerciseService.delete(id);
        return ResponseEntity.ok("Exercise deleted successfully");
    }

    @GetMapping("/exercises/findByMuscleGroup")
    public ResponseEntity<List<ExerciseResponseDto>> findByMuscleGroup(@RequestParam String muscleGroup){
        return ResponseEntity.ok(exerciseService.findByMuscleGroup(muscleGroup));
    }

    @GetMapping("/exercises/getExerciseDescription")
    public ResponseEntity<String> getExerciseDescription(@RequestParam String exName){
        return ResponseEntity.ok(exerciseService.getExerciseDescription(exName));
    }

}
