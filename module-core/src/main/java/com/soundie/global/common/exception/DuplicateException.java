package com.soundie.global.common.exception;

public class DuplicateException extends ApplicationException {

    public DuplicateException(ApplicationError error) {
        super(error);
    }
}
