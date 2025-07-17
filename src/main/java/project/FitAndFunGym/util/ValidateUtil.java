package project.FitAndFunGym.util;

import io.micrometer.common.util.StringUtils;
import project.FitAndFunGym.exception.BadRequestException;

import java.util.Objects;

public class ValidateUtil {

    public static void isValid(Long id){
        if(Objects.isNull(id)){
            throw new BadRequestException("Id cannot be null");
        }
    }

    public static void isValid(String value, String fieldName) {
        if (StringUtils.isBlank(value)) {
            throw new BadRequestException(String.format("%s cannot be null or blank", fieldName));
        }
    }
}
