package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class GetChatRoomDetailElement {

    private Long chatRoomId;
    private String name;
    private String description;
    private LocalDateTime createdAt;

    public static GetChatRoomDetailElement of(ChatRoom chatRoom) {
        return GetChatRoomDetailElement.builder()
                .chatRoomId(chatRoom.getId())
                .name(chatRoom.getName())
                .description(chatRoom.getDescription())
                .createdAt(chatRoom.getCreatedAt())
                .build();
    }
}
