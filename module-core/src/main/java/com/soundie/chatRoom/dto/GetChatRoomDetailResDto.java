package com.soundie.chatRoom.dto;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

import java.util.List;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetChatRoomDetailResDto {

    private final GetChatRoomDetailElement chatRoom;
    private final List<ChatMessage> chatMessages;

    private static GetChatRoomDetailResDtoBuilder builder(GetChatRoomDetailElement chatRoom, List<ChatMessage> chatMessages) {
        return innerBuilder()
                .chatRoom(chatRoom)
                .chatMessages(chatMessages);
    }

    public static GetChatRoomDetailResDto of(ChatRoom chatRoom, List<ChatMessage> chatMessages) {
        return GetChatRoomDetailResDto.builder(
                    GetChatRoomDetailElement.of(chatRoom),
                    chatMessages
                )
                .build();
    }
}
