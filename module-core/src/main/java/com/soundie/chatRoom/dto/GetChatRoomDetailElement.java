package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetChatRoomDetailElement {

    private final Long chatRoomId;
    private final String name;
    private final String description;
    private final LocalDateTime createdAt;

    private static GetChatRoomDetailElementBuilder builder(
            Long chatRoomId,
            String name,
            String description,
            LocalDateTime createdAt) {
        return innerBuilder()
                .chatRoomId(chatRoomId)
                .name(name)
                .description(description)
                .createdAt(createdAt);
    }

    public static GetChatRoomDetailElement of(ChatRoom chatRoom) {
        return GetChatRoomDetailElement.builder(
                    chatRoom.getId(),
                    chatRoom.getName(),
                    chatRoom.getDescription(),
                    chatRoom.getCreatedAt()
                )
                .build();
    }
}
