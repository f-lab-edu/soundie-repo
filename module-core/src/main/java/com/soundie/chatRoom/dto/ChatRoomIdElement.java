package com.soundie.chatRoom.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class ChatRoomIdElement {

    private final Long chatRoomId;

    public static ChatRoomIdElement of(Long chatRoomId){
        return ChatRoomIdElement.builder()
                .chatRoomId(chatRoomId)
                .build();
    }
}
