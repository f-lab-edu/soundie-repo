package com.soundie.global.common.dto;

import lombok.*;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EnvelopeResponse<T> {

    @Builder.Default
    private String code = "200";

    @Builder.Default
    private String message = "success";

    private T data;
}
