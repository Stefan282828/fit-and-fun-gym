package project.FitAndFunGym.controller;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.dto.UserDto.UserRequestDto;
import project.FitAndFunGym.dto.UserDto.UserResponseDto;
import project.FitAndFunGym.dto.UserTrPlanExResponseDto;
import project.FitAndFunGym.service.UserService;
import project.FitAndFunGym.service.UserTrainingPlanService;

import java.util.List;

@RestController
@RequestMapping(value = "/project")
public class UserController {

    private final UserService userService;
    private final UserTrainingPlanService userTrainingPlanService;

    public UserController(UserService userService, UserTrainingPlanService userTrainingPlanService) {
        this.userService = userService;
        this.userTrainingPlanService = userTrainingPlanService;
    }

    @GetMapping(value = "/users")
    public ResponseEntity<List<UserResponseDto>> getAll() {
        return ResponseEntity.ok(userService.getAll());
    }

    @GetMapping(value = "/users/{id}")
    public ResponseEntity<UserResponseDto> getById(@PathVariable Long id) {
        return ResponseEntity.ok(userService.getById(id));
    }

    @PostMapping(value = "/users/addUser")
    public ResponseEntity<UserResponseDto> addUser(@RequestBody UserRequestDto userRequestDto) {
        return ResponseEntity.status(HttpStatus.CREATED).body(userService.createUser(userRequestDto));
    }

    @DeleteMapping(value = "/users/deleteById")
    public ResponseEntity<String> deleteById(@RequestParam Long id) {
        userService.deleteUser(id);
        return ResponseEntity.ok("User deleted successfully");
    }

    @PutMapping(value ="/users/update")
    public ResponseEntity<UserResponseDto> update(@RequestBody UserRequestDto userRequestDto) {
        return ResponseEntity.ok(userService.updateUser(userRequestDto));
    }

    @GetMapping(value = "/users/search")
    public ResponseEntity<Page<UserResponseDto>> search (@RequestBody UserRequestDto userRequestDto,
                                                         @RequestParam(name = "page", defaultValue = "0") int page,
                                                         @RequestParam(name = "size", defaultValue = "2") int size,
                                                         @RequestParam(name = "sortField", defaultValue = "id") String sortField,
                                                         @RequestParam(name = "sortDirection", defaultValue = "ASC") String sortDirection){
        return ResponseEntity.ok(userService.searchUsers(userRequestDto, page, size, sortField, sortDirection));
    }

    @PostMapping(value = "/users/assignTrainingPlan")
    public ResponseEntity<String> assignTrainingPlan (@RequestParam Long userId, @RequestParam Long trainingPlanId){
        userService.assignTrainingPlan(userId, trainingPlanId);
        return ResponseEntity.ok("Training plan assigned successfully");
    }

    @GetMapping(value = "/users/getTrPlanExercises/{userId}")
    public ResponseEntity<UserTrPlanExResponseDto> getTrPlanExercises (@PathVariable Long userId){
        return ResponseEntity.ok(userTrainingPlanService.getTrPlanExercises(userId));
    }

    @PutMapping(value = "/users/finishTrainingPlan/{userId}")
    public ResponseEntity<String> finishTrainingPlan(@PathVariable Long userId){
        userService.finishTrainingPlan(userId);
        return ResponseEntity.ok("You finished your training plan. Congrats!");
    }
}
