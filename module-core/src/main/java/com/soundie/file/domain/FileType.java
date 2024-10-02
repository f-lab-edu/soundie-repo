package com.soundie.file.domain;

import lombok.Getter;

@Getter
public enum FileType {

    POST_MUSIC("POST_MUSIC"),
    POST_IMG("POST_IMG"),
    CHAT_IMG("CHAT_IMG");

    private final String name;

    FileType(String name) {
        this.name = name;
    }
}
