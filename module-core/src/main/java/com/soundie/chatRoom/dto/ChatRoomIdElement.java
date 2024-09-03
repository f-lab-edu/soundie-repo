package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class ChatRoomIdElement {

    private final Long chatRoomId;

    private static ChatRoomIdElementBuilder builder(Long chatRoomId){
        return innerBuilder()
                .chatRoomId(chatRoomId);
    }

    public static ChatRoomIdElement of(ChatRoom chatRoom){
        return ChatRoomIdElement.builder(
                    chatRoom.getId()
                )
                .build();
    }

    public static ChatRoomIdElement ofId(Long chatRoomId){
        return ChatRoomIdElement.builder(
                    chatRoomId
                )
                .build();
    }
}
