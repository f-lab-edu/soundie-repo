package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetChatRoomDetailElement {

    private final Long chatRoomId;
    private final String name;
    private final String description;
    private final LocalDateTime createdAt;

    public static GetChatRoomDetailElement of(ChatRoom chatRoom) {
        return new GetChatRoomDetailElement(
                    chatRoom.getId(),
                    chatRoom.getName(),
                    chatRoom.getDescription(),
                    chatRoom.getCreatedAt()
                );
    }
}
