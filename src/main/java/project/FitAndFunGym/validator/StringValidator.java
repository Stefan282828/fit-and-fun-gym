package project.FitAndFunGym.validator;

import io.micrometer.common.util.StringUtils;
import project.FitAndFunGym.exception.BadRequestException;

public class StringValidator {

    public static void validateString(String value, String fieldName) {
        if (StringUtils.isBlank(value)) {
            throw new BadRequestException(String.format("%s cannot be null or blank", fieldName));
        }
    }
}
