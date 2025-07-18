package project.FitAndFunGym.service;

import com.querydsl.core.BooleanBuilder;
import io.micrometer.common.util.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import project.FitAndFunGym.dto.UserDto.UserRequestDto;
import project.FitAndFunGym.dto.UserDto.UserResponseDto;
import project.FitAndFunGym.entity.*;
import project.FitAndFunGym.mapper.UserMapper;
import project.FitAndFunGym.repository.TrainingPlanRepository;
import project.FitAndFunGym.repository.UserRepository;
import project.FitAndFunGym.repository.UserTrainingPlanRepository;
import project.FitAndFunGym.validator.TrainingPlanValidator;
import project.FitAndFunGym.validator.UserTrainingPlanValidator;
import project.FitAndFunGym.validator.UserValidator;

import java.util.*;

@Service
public class UserService {

    private final UserRepository userRepository;
    private final UserValidator  userValidator;
    private final TrainingPlanRepository trainingPlanRepository;
    private final TrainingPlanValidator trainingPlanValidator;
    private final UserTrainingPlanRepository userTrainingPlanRepository;
    private final UserTrainingPlanValidator userTrainingPlanValidator;
    private  BCryptPasswordEncoder encoder = new BCryptPasswordEncoder(12);

    public UserService(UserRepository userRepository, UserValidator userValidator, TrainingPlanRepository trainingPlanRepository, TrainingPlanValidator trainingPlanValidator, UserTrainingPlanRepository userTrainingPlanRepository, UserTrainingPlanValidator userTrainingPlanValidator) {
        this.userRepository = userRepository;
        this.userValidator = userValidator;
        this.trainingPlanRepository = trainingPlanRepository;
        this.trainingPlanValidator = trainingPlanValidator;
        this.userTrainingPlanRepository = userTrainingPlanRepository;
        this.userTrainingPlanValidator = userTrainingPlanValidator;
    }

    @Transactional(readOnly = true)
    public Page<UserResponseDto> getAll(int page, int size, String sortField, String sortDirection){
        Sort sort = Sort.by(Sort.Direction.fromString(sortDirection), sortField);
        Pageable pageable = PageRequest.of(page, size, sort);
        return userRepository.findAll(pageable).map(UserMapper::toDto);
    }

    @Transactional(readOnly = true)
    public UserResponseDto getById(Long id){
        userValidator.doesExist(id);
        User user = userRepository.findById(id).get();
        return UserMapper.toDto(user);
    }

    @Transactional
    public UserResponseDto createUser(UserRequestDto userRequestDto){
        userValidator.validCreate(userRequestDto);
        User user = UserMapper.toEntity(userRequestDto);
        user.setPassword(encoder.encode(user.getPassword()));
        userRepository.save(user);
        return UserMapper.toDto(user);
    }

    @Transactional
    public void deleteUser(Long id){
        userValidator.doesExist(id);
        userRepository.deleteById(id);
    }

    @Transactional
    public UserResponseDto updateUser(UserRequestDto userRequestDto){
        userValidator.validUpdate(userRequestDto);
        User user = updateValues(userRequestDto);
        return UserMapper.toDto(userRepository.save(user));
    }

    @Transactional(readOnly = true)
    public Page<UserResponseDto> searchUsers(UserRequestDto userRequestDto, int page, int size, String sortField, String sortDirection){
        userValidator.isValidUser(userRequestDto);
        User user = UserMapper.toEntity(userRequestDto);
        QUser qUser = QUser.user;
        Sort sort = Sort.by(Sort.Direction.fromString(sortDirection), sortField);
        Pageable pageable = PageRequest.of(page, size, sort);
        BooleanBuilder booleanBuilder = new BooleanBuilder();

        if (StringUtils.isNotBlank(user.getName())) {
            booleanBuilder.and(qUser.name.startsWithIgnoreCase(user.getName()));
        }
        if (StringUtils.isNotBlank(user.getLastName())) {
            booleanBuilder.and(qUser.lastName.startsWithIgnoreCase(user.getLastName()));
        }
        if (StringUtils.isNotBlank(user.getEmail())) {
            booleanBuilder.and(qUser.email.eq(user.getEmail()));
        }
        if(StringUtils.isNotBlank(user.getPassword())){
            booleanBuilder.and(qUser.password.eq(user.getPassword()));
        }
        if(StringUtils.isNotBlank(user.getUsername())){
            booleanBuilder.and(qUser.username.eq(user.getUsername()));
        }
        if(Objects.nonNull(user.getDateOfBirth())){
            booleanBuilder.and(qUser.dateOfBirth.eq(user.getDateOfBirth()));
        }

        return userRepository.findAll(booleanBuilder,pageable).map(UserMapper::toDto);
    }

    @Transactional
    public User updateValues(UserRequestDto userRequestDto){
        User user = userRepository.findById(userRequestDto.getId()).get();
        if (StringUtils.isNotBlank(userRequestDto.getName())){
            user.setName(userRequestDto.getName());
        }
        if (StringUtils.isNotBlank(userRequestDto.getEmail())){
            user.setEmail(userRequestDto.getEmail());
        }
        if (StringUtils.isNotBlank(userRequestDto.getPassword())){
            user.setPassword(userRequestDto.getPassword());
        }
        if (StringUtils.isNotBlank(userRequestDto.getLastName())){
            user.setLastName(userRequestDto.getLastName());
        }
        if (StringUtils.isNotBlank(userRequestDto.getUsername())){
            user.setUsername(userRequestDto.getUsername());
        }
        if (Objects.nonNull(userRequestDto.getDateOfBirth())) {
            userRequestDto.setDateOfBirth(userRequestDto.getDateOfBirth());
        }
        return user;
    }

    @Transactional
    public void assignTrainingPlan(Long userId, Long trainingPlanId){
        userValidator.doesExist(userId);
        trainingPlanValidator.doesExist(trainingPlanId);
        userTrainingPlanValidator.hasActivePlan(userId);
        User user = userRepository.findById(userId).get();
        TrainingPlan trainingPlan = trainingPlanRepository.findById(trainingPlanId).get();
        UserTrainingPlan userTrainingPlan = new UserTrainingPlan(user, trainingPlan);
        userTrainingPlanRepository.save(userTrainingPlan);
    }

    @Transactional
    public void finishTrainingPlan(Long userId){
        userValidator.doesExist(userId);
        userTrainingPlanValidator.doesHaveActivePlan(userId);
        UserTrainingPlan activePlan = userTrainingPlanRepository.findByUser_IdAndStatus(userId,Status.ACTIVE).get();
        activePlan.setStatus(Status.FINISHED);
    }

}
