package com.soundie.global.common.exception;

public class BadRequestException extends ApplicationException {

    public BadRequestException(ApplicationError error) {
        super(error);
    }
}
