package com.soundie.global.common;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class EnvelopeResponse<T> {

    private String code;
    private String message;
    private T data;
}
