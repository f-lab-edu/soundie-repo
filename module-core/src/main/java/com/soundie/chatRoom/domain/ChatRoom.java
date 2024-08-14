package com.soundie.chatRoom.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ChatRoom {
    private Long id;
    private Long memberId;
    private String name;
    private String description;
    private LocalDateTime createdAt;

    public ChatRoom(Long memberId, String name, String description) {
        this.memberId = memberId;
        this.name = name;
        this.description = description;
        this.createdAt = LocalDateTime.now();
    }
}
