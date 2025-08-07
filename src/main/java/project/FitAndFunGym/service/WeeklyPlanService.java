package project.FitAndFunGym.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.dto.WeeklyPlanDto.DailyExerciseResponseDto;
import project.FitAndFunGym.dto.WeeklyPlanDto.DailyMealResponseDto;
import project.FitAndFunGym.dto.WeeklyPlanDto.DailyPlanResponseDto;
import project.FitAndFunGym.dto.WeeklyPlanDto.WeeklyPlanResponseDto;
import project.FitAndFunGym.entity.*;
import project.FitAndFunGym.repository.DailyPlanRepository;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.validator.TrainingPlanValidator;

import java.util.*;
import java.util.stream.Collectors;

@Service
public class WeeklyPlanService {

    private final DailyPlanRepository dailyPlanRepository;
    private final TrainingPlanRepository trainingPlanRepository;
    private final TrainingPlanValidator trainingPlanValidator;

    public WeeklyPlanService(DailyPlanRepository dailyPlanRepository,
                            TrainingPlanRepository trainingPlanRepository,
                            TrainingPlanValidator trainingPlanValidator) {
        this.dailyPlanRepository = dailyPlanRepository;
        this.trainingPlanRepository = trainingPlanRepository;
        this.trainingPlanValidator = trainingPlanValidator;
    }

    @Transactional(readOnly = true)
    public WeeklyPlanResponseDto getWeeklyPlan(Long trainingPlanId) {
        trainingPlanValidator.doesExist(trainingPlanId);
        
        TrainingPlan trainingPlan = trainingPlanRepository.findById(trainingPlanId).get();
        List<DailyPlan> dailyPlans = dailyPlanRepository.findCompleteWeeklyPlan(trainingPlanId);

        Map<DayOfWeek, DailyPlanResponseDto> dailyPlanMap = dailyPlans.stream()
                .collect(Collectors.toMap(
                        DailyPlan::getDayOfWeek,
                        this::convertToDailyPlanResponseDto
                ));

        return new WeeklyPlanResponseDto(
                trainingPlan.getId(),
                trainingPlan.getName(),
                trainingPlan.getGoal(),
                trainingPlan.getDifficulty(),
                trainingPlan.getDuration(),
                trainingPlan.getDescription(),
                dailyPlanMap
        );
    }

    @Transactional(readOnly = true)
    public DailyPlanResponseDto getDailyPlan(Long trainingPlanId, DayOfWeek dayOfWeek) {
        trainingPlanValidator.doesExist(trainingPlanId);
        
        Optional<DailyPlan> dailyPlanOpt = dailyPlanRepository.findByTrainingPlanIdAndDayOfWeek(trainingPlanId, dayOfWeek);
        
        if (dailyPlanOpt.isEmpty()) {
            return new DailyPlanResponseDto(null, dayOfWeek, "Rest Day", "No specific workout planned", 
                                          new ArrayList<>(), new HashMap<>());
        }

        return convertToDailyPlanResponseDto(dailyPlanOpt.get());
    }

    private DailyPlanResponseDto convertToDailyPlanResponseDto(DailyPlan dailyPlan) {
        List<DailyExerciseResponseDto> exercises = dailyPlan.getExercises().stream()
                .sorted(Comparator.comparing(DailyExercise::getOrderInWorkout, Comparator.nullsLast(Comparator.naturalOrder())))
                .map(this::convertToDailyExerciseResponseDto)
                .collect(Collectors.toList());

        Map<MealType, List<DailyMealResponseDto>> mealsByType = dailyPlan.getMeals().stream()
                .map(this::convertToDailyMealResponseDto)
                .collect(Collectors.groupingBy(DailyMealResponseDto::getMealType));

        return new DailyPlanResponseDto(
                dailyPlan.getId(),
                dailyPlan.getDayOfWeek(),
                dailyPlan.getFocusArea(),
                dailyPlan.getNotes(),
                exercises,
                mealsByType
        );
    }

    private DailyExerciseResponseDto convertToDailyExerciseResponseDto(DailyExercise dailyExercise) {
        Exercise exercise = dailyExercise.getExercise();
        return new DailyExerciseResponseDto(
                dailyExercise.getId(),
                exercise.getName(),
                exercise.getDescription(),
                exercise.getMuscleGroup(),
                exercise.getEquipmentNeeded(),
                dailyExercise.getSets(),
                dailyExercise.getReps(),
                dailyExercise.getWeight(),
                dailyExercise.getRestTime(),
                dailyExercise.getOrderInWorkout(),
                dailyExercise.getNotes()
        );
    }

    private DailyMealResponseDto convertToDailyMealResponseDto(DailyMeal dailyMeal) {
        Food food = dailyMeal.getFood();
        Double quantity = dailyMeal.getQuantity();
        
        // Calculate nutritional values based on quantity
        Double totalCalories = calculateNutrition(food.getCaloriesPer100g(), quantity);
        Double totalProtein = calculateNutrition(food.getProteinPer100g(), quantity);
        Double totalCarbs = calculateNutrition(food.getCarbsPer100g(), quantity);
        Double totalFat = calculateNutrition(food.getFatPer100g(), quantity);

        return new DailyMealResponseDto(
                dailyMeal.getId(),
                food.getName(),
                food.getDescription(),
                food.getCategory(),
                dailyMeal.getMealType(),
                quantity,
                dailyMeal.getNotes(),
                totalCalories,
                totalProtein,
                totalCarbs,
                totalFat
        );
    }

    private Double calculateNutrition(Double per100g, Double quantity) {
        if (per100g == null || quantity == null) {
            return null;
        }
        return (per100g * quantity) / 100.0;
    }
}