package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.DailyPlan;
import project.FitAndFunGym.entity.DayOfWeek;

import java.util.List;
import java.util.Optional;

@Repository
public interface DailyPlanRepository extends JpaRepository<DailyPlan, Long> {

    List<DailyPlan> findByTrainingPlanIdOrderByDayOfWeek(Long trainingPlanId);

    Optional<DailyPlan> findByTrainingPlanIdAndDayOfWeek(Long trainingPlanId, DayOfWeek dayOfWeek);

    @Query("SELECT dp FROM DailyPlan dp " +
           "LEFT JOIN FETCH dp.exercises de " +
           "LEFT JOIN FETCH de.exercise " +
           "LEFT JOIN FETCH dp.meals dm " +
           "LEFT JOIN FETCH dm.food " +
           "WHERE dp.trainingPlan.id = :trainingPlanId " +
           "ORDER BY dp.dayOfWeek")
    List<DailyPlan> findCompleteWeeklyPlan(@Param("trainingPlanId") Long trainingPlanId);
}