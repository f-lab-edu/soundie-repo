package com.soundie.chatRoom.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostChatRoomCreateReqDto {
    private String name;
    private String description;

    public PostChatRoomCreateReqDto(String name, String description){
        this.name = name;
        this.description = description;
    }
}
