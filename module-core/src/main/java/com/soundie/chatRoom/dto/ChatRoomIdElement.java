package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ChatRoomIdElement {

    private final Long chatRoomId;

    public static ChatRoomIdElement of(ChatRoom chatRoom){
        return new ChatRoomIdElement(chatRoom.getId());
    }
}
