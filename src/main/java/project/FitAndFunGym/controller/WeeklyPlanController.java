package project.FitAndFunGym.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import project.FitAndFunGym.dto.WeeklyPlanDto.DailyPlanResponseDto;
import project.FitAndFunGym.dto.WeeklyPlanDto.WeeklyPlanResponseDto;
import project.FitAndFunGym.entity.DayOfWeek;
import project.FitAndFunGym.service.WeeklyPlanService;

@RestController
@RequestMapping("/project")
public class WeeklyPlanController {

    private final WeeklyPlanService weeklyPlanService;

    public WeeklyPlanController(WeeklyPlanService weeklyPlanService) {
        this.weeklyPlanService = weeklyPlanService;
    }

    @GetMapping("/weekly-plan/{trainingPlanId}")
    @PreAuthorize("hasAnyRole('USER', 'COACH', 'ADMIN')")
    public ResponseEntity<WeeklyPlanResponseDto> getWeeklyPlan(@PathVariable Long trainingPlanId) {
        return ResponseEntity.ok(weeklyPlanService.getWeeklyPlan(trainingPlanId));
    }

    @GetMapping("/daily-plan/{trainingPlanId}/{dayOfWeek}")
    @PreAuthorize("hasAnyRole('USER', 'COACH', 'ADMIN')")
    public ResponseEntity<DailyPlanResponseDto> getDailyPlan(
            @PathVariable Long trainingPlanId,
            @PathVariable DayOfWeek dayOfWeek) {
        return ResponseEntity.ok(weeklyPlanService.getDailyPlan(trainingPlanId, dayOfWeek));
    }
}