package com.soundie.chatRoom.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostChatRoomCreateReqDto {
    private String name;
    private String description;
}
