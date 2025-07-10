package project.FitAndFunGym.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import project.FitAndFunGym.service.UserTrainingPlanService;

@RestController
@RequestMapping(value = "/project")
public class UserTrainingPlanController {

    private final UserTrainingPlanService userTrainingPlanService;

    public UserTrainingPlanController(UserTrainingPlanService userTrainingPlanService) {
        this.userTrainingPlanService = userTrainingPlanService;
    }

    @DeleteMapping(value = "/userTrPlans/delete")
    public ResponseEntity<String> delete(@RequestParam Long userId, @RequestParam Long trainingPlanId){
        userTrainingPlanService.delete(userId,trainingPlanId);
        return ResponseEntity.ok("Deleted");
    }
}
