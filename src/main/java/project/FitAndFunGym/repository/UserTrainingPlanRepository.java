package project.FitAndFunGym.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.dto.UserTrPlanExRequestDto;
import project.FitAndFunGym.entity.Status;
import project.FitAndFunGym.entity.UserTrainingPlan;
import project.FitAndFunGym.entity.UserTrainingPlanId;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserTrainingPlanRepository extends JpaRepository<UserTrainingPlan, UserTrainingPlanId> {

    boolean existsByUser_Id(Long userId);

    @Query("SELECT new project.FitAndFunGym.dto.UserTrPlanExRequestDto("
            + " u.id, u.name, u.lastName, tp.name, e.name) "
            + " FROM UserTrainingPlan utp "
            + " JOIN utp.user u "
            + " JOIN utp.trainingPlan tp "
            + " JOIN tp.exercises e "
            + " WHERE u.id = :userId")
    List<UserTrPlanExRequestDto> findTrPlanExercisesByUserId(Long userId);

    Optional<UserTrainingPlan> findByUser_IdAndStatus(Long userId, Status staus);
}
