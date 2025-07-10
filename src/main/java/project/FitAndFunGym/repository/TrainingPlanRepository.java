package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.TrainingPlan;

@Repository
public interface TrainingPlanRepository  extends JpaRepository<TrainingPlan, Long> {

    boolean existsByName(String name);
}
