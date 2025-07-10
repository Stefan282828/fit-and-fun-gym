package project.FitAndFunGym.repository;

import jakarta.persistence.EntityManager;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;
import project.FitAndFunGym.entity.User;



@Repository
public interface UserRepository extends JpaRepository<User, Long>, PagingAndSortingRepository<User, Long>
                                        ,QuerydslPredicateExecutor<User> {

    boolean existsByUsername(String username);
    User findByUsername (String username);
}