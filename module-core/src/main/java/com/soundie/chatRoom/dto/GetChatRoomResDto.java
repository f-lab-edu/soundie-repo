package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder
public class GetChatRoomResDto {

    private Collection<GetChatRoomElement> chatRooms;

    public static GetChatRoomResDto of(List<ChatRoom> chatRooms) {
        return GetChatRoomResDto.builder()
                .chatRooms(chatRooms.stream()
                        .map(cr -> GetChatRoomElement.of(cr))
                        .collect(Collectors.toList()))
                .build();

    }
}
