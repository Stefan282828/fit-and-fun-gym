package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.DailyMeal;
import project.FitAndFunGym.entity.MealType;

import java.util.List;

@Repository
public interface DailyMealRepository extends JpaRepository<DailyMeal, Long> {

    List<DailyMeal> findByDailyPlanIdOrderByMealType(Long dailyPlanId);

    List<DailyMeal> findByDailyPlanIdAndMealType(Long dailyPlanId, MealType mealType);
}