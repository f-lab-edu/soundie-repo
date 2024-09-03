package com.soundie.chatRoom.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
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
    }

    /*
     * MemoryRepository 저장 위함
     * */
    public void setId(Long id) {
        this.id = id;
    }
}
