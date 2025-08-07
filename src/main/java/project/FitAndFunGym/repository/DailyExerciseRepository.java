package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.DailyExercise;

import java.util.List;

@Repository
public interface DailyExerciseRepository extends JpaRepository<DailyExercise, Long> {

    List<DailyExercise> findByDailyPlanIdOrderByOrderInWorkout(Long dailyPlanId);
}