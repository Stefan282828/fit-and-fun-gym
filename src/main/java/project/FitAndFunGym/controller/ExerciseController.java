package project.FitAndFunGym.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.entity.Exercise;
import project.FitAndFunGym.service.ExerciseService;

import java.util.List;

@RestController
@RequestMapping("/project/exercises")
public class ExerciseController {

    private final ExerciseService exerciseService;

    public ExerciseController(ExerciseService exerciseService) {
        this.exerciseService = exerciseService;
    }

    @GetMapping
    public ResponseEntity<List<Exercise>> getAll() {
        return ResponseEntity.ok(exerciseService.getAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<Exercise> getById(@PathVariable Long id) {
        return ResponseEntity.ok(exerciseService.getById(id));
    }

    @PostMapping("/add")
    public ResponseEntity<Exercise> create(@RequestBody Exercise exercise) {
        return ResponseEntity.ok(exerciseService.create(exercise));
    }

//    @PutMapping("/update")
//    public ResponseEntity<Exercise> update(@RequestBody Exercise exercise) {
//        return ResponseEntity.ok(exerciseService.update(exercise));
//    }

    @DeleteMapping("/delete")
    public ResponseEntity<String> delete(@RequestParam Long id) {
        exerciseService.delete(id);
        return ResponseEntity.ok("Exercise deleted successfully");
    }

}
