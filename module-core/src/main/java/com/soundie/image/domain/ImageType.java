package com.soundie.image.domain;

import lombok.Getter;

@Getter
public enum ImageType {

    POST("POST"),
    CHAT("CHAT");

    private final String name;

    ImageType(String name) {
        this.name = name;
    }
}
