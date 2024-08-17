package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class GetChatRoomDetailResDto {

    private GetChatRoomDetailElement chatRoom;

    public static GetChatRoomDetailResDto of(ChatRoom chatRoom) {
        return GetChatRoomDetailResDto.builder()
                .chatRoom(GetChatRoomDetailElement.of(chatRoom))
                .build();
    }
}
