package project.FitAndFunGym.validator;

import project.FitAndFunGym.exception.BadRequestException;

import java.util.Objects;

public class IsValidValidator {

    public static void isValidId(Long id){
        if(Objects.isNull(id)){
            throw new BadRequestException("Id cannot be null");
        }
    }
}
