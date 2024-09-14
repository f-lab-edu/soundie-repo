package com.soundie.chatRoom.dto;

import com.soundie.chatRoom.domain.ChatRoom;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetChatRoomResDto {

    private final Collection<GetChatRoomElement> chatRooms;

    public static GetChatRoomResDto of(List<ChatRoom> chatRooms) {
        return new GetChatRoomResDto(
                    chatRooms.stream()
                            .map(GetChatRoomElement::of)
                            .collect(Collectors.toList())
                );

    }
}
