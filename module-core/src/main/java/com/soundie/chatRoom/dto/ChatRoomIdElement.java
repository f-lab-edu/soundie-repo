package com.soundie.chatRoom.dto;

import lombok.Getter;

@Getter
public class ChatRoomIdElement {

    private final Long chatRoomId;

    public ChatRoomIdElement(Long chatRoomId){
        this.chatRoomId = chatRoomId;
    }
}
