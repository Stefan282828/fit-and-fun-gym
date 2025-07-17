package project.FitAndFunGym.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.dto.ExerciseDto.ExerciseResponseDto;
import project.FitAndFunGym.entity.Exercise;

import java.util.List;

@Repository
public interface ExerciseRepository extends JpaRepository<Exercise, Long> {

    @Query("SELECT COUNT(e) > 0 FROM Exercise e" +
            " WHERE LOWER(e.name) = LOWER(:name)")
    boolean existsByName(String name);

    @Query("SELECT new project.FitAndFunGym.dto.ExerciseDto.ExerciseResponseDto("
            + " e.name, e.description, e.muscleGroup) "
            + " FROM Exercise e"
            + " WHERE LOWER(e.muscleGroup) LIKE LOWER(CONCAT('%', :muscleGroup, '%'))")
    List<ExerciseResponseDto> findByMuscleGroupContaining(@Param("muscleGroup") String muscleGroup);

    @Query("SELECT e.description FROM Exercise e " +
            "WHERE LOWER(TRIM(e.name)) = LOWER(TRIM(:exerciseName))")
    String getExerciseDescription(@Param("exerciseName") String exerciseName);
}
