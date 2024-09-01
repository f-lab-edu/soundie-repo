package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetChatRoomDetailResDto {

    private final GetChatRoomDetailElement chatRoom;

    private static GetChatRoomDetailResDtoBuilder builder(GetChatRoomDetailElement chatRoom) {
        return innerBuilder()
                .chatRoom(chatRoom);
    }

    public static GetChatRoomDetailResDto of(ChatRoom chatRoom) {
        return GetChatRoomDetailResDto.builder(
                    GetChatRoomDetailElement.of(chatRoom)
                )
                .build();
    }
}
