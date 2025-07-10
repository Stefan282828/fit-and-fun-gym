package project.FitAndFunGym.mapper;

import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import project.FitAndFunGym.dto.UserDto.UserRequestDto;
import project.FitAndFunGym.dto.UserDto.UserResponseDto;
import project.FitAndFunGym.entity.User;
import project.FitAndFunGym.exception.BadRequestException;
import java.util.List;
import java.util.Objects;

@Component
public class UserMapper {

    public static User toEntity (UserRequestDto userRequestDto){
        if (Objects.isNull(userRequestDto)){
            throw new BadRequestException("UserRequestDto is null");
        }
        return new User(userRequestDto.getName(),
                userRequestDto.getLastName(),
                userRequestDto.getUsername(),
                userRequestDto.getEmail(),
                userRequestDto.getPassword(),
                userRequestDto.getDateOfBirth(),
                userRequestDto.getRole());

    }

    public static UserResponseDto toDto (User user){
        if (Objects.isNull(user)){
            throw new BadRequestException("User is null");
        }
        return new UserResponseDto(user.getId(),
                user.getName(),
                user.getLastName(),
                user.getUsername(),
                user.getEmail(),
                user.getDateOfBirth(),
                user.getRole());
    }

    public static List<User> toEntityList (List<UserRequestDto> userRequestDtoList){
        if (ObjectUtils.isEmpty(userRequestDtoList)){
            throw new BadRequestException("UserRequestDto list is null");
        }
        return userRequestDtoList.stream().map(UserMapper::toEntity).toList();
    }

    public static List<UserResponseDto> toDtoList (List<User> userList){
        if (ObjectUtils.isEmpty(userList)){
            throw new BadRequestException("UserRequestDto list is null");
        }
        return userList.stream().map(UserMapper::toDto).toList();
    }
}
