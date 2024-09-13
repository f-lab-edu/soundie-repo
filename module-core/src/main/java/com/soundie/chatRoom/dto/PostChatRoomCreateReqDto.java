package com.soundie.chatRoom.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostChatRoomCreateReqDto {

    private Long guestMemberId;
    private String name;
    private String description;

    public PostChatRoomCreateReqDto(Long guestMemberId, String name, String description){
        this.guestMemberId = guestMemberId;
        this.name = name;
        this.description = description;
    }
}
