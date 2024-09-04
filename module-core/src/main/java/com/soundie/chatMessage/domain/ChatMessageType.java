package com.soundie.chatMessage.domain;

import lombok.Getter;

@Getter
public enum ChatMessageType {
    ENTER("ENTER"),
    TALK("TALK");

    private final String name;

    ChatMessageType(String name) {
        this.name = name;
    }
}
