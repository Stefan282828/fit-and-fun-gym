package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.TrainingPlan;

import java.util.List;

@Repository
public interface TrainingPlanRepository  extends JpaRepository<TrainingPlan, Long> {

    boolean existsByName(String name);

    @Query("SELECT e.name FROM TrainingPlan tp " +
            " JOIN tp.exercises e" +
            " WHERE LOWER(tp.name) = LOWER(:trainingPlanName)")
    List<String> findExercisesByTrPlanName(@Param("trainingPlanName") String trainingPlanName);

}
