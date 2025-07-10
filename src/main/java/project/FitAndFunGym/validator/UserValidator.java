package project.FitAndFunGym.validator;

import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import project.FitAndFunGym.dto.UserDto.UserRequestDto;
import project.FitAndFunGym.exception.BadRequestException;
import project.FitAndFunGym.repository.UserRepository;

import java.time.LocalDate;
import java.util.Objects;

@Service
public class UserValidator {

    private final UserRepository userRepository;

    public UserValidator(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void isValidUser(UserRequestDto userRequestDto){
        if(Objects.isNull(userRequestDto)){
            throw new BadRequestException("UserRequestDto cannot be null");
        }
    }

    public void doesExistById(Long id){
        IsValidValidator.isValidId(id);
        if(Boolean.FALSE.equals(userRepository.existsById(id))){
            throw new BadRequestException(String.format("User with id %s not found", id));
        }
    }

    public void doesExistByUsername(String username){
        StringValidator.validateString(username, "username");
        if(Boolean.FALSE.equals(userRepository.existsByUsername(username))){
            throw new UsernameNotFoundException(String.format("User with username %s not found", username));
        }
    }

    public void alreadyExists(UserRequestDto userRequestDto){
        isValidUser(userRequestDto);
        if(Boolean.TRUE.equals(userRepository.existsByUsername(userRequestDto.getUsername()))){
            throw new BadRequestException(String.format("User with username %s already exists", userRequestDto.getUsername()));
        }
    }

    private void validateDateOfBirth(LocalDate birthDate){
        if(Objects.isNull(birthDate)){
            throw new BadRequestException("Date of birth cannot be null");
        }
    }

    public void validCreate(UserRequestDto userRequestDto){
        StringValidator.validateString(userRequestDto.getName(), "Name");
        StringValidator.validateString(userRequestDto.getLastName(), "Last name");
        StringValidator.validateString(userRequestDto.getUsername(), "Username");
        StringValidator.validateString(userRequestDto.getEmail(), "Email");
        StringValidator.validateString(userRequestDto.getPassword(), "Password");
        validateDateOfBirth(userRequestDto.getDateOfBirth());
        alreadyExists(userRequestDto);
    }

    public void validUpdate(UserRequestDto userRequestDto){
        doesExistById(userRequestDto.getId());
        alreadyExists(userRequestDto);
    }

}
