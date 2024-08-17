package com.soundie.chatRoom.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ChatRoom {
    private Long id;
    private Long hostMemberId;
    private Long guestMemberId;
    private String name;
    private String description;
    private LocalDateTime createdAt;

    public ChatRoom(Long hostMemberId, Long guestMemberId, String name, String description) {
        this.hostMemberId = hostMemberId;
        this.guestMemberId = guestMemberId;
        this.name = name;
        this.description = description;
        this.createdAt = LocalDateTime.now();
    }
}
