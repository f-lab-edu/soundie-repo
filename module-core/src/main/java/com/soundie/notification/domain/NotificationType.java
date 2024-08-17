package com.soundie.notification.domain;

import lombok.Getter;

@Getter
public enum NotificationType {
    SYSTEM(1, "SYSTEM"),
    POST_LIKE(2, "POST_LIKE"),
    POST_REPLY(3, "POST_REPLY"),
    CHAT_ROOM(4, "CHAT_ROOM"),
    CHAT_MESSAGE(5, "CHAT_MESSAGE");


    private final Integer id;
    private final String name;

    NotificationType(Integer id, String name){
        this.id = id;
        this.name = name;
    }
}
