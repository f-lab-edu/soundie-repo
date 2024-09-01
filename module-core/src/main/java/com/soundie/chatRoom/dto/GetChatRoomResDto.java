package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetChatRoomResDto {

    private final Collection<GetChatRoomElement> chatRooms;

    private static GetChatRoomResDtoBuilder builder(Collection<GetChatRoomElement> chatRooms) {
        return innerBuilder()
                .chatRooms(chatRooms);
    }

    public static GetChatRoomResDto of(List<ChatRoom> chatRooms) {
        return GetChatRoomResDto.builder(
                    chatRooms.stream()
                            .map(GetChatRoomElement::of)
                            .collect(Collectors.toList())
                )
                .build();

    }
}
