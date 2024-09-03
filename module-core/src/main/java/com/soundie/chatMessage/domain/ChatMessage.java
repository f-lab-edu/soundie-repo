package com.soundie.chatMessage.domain;

import lombok.Getter;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
public class ChatMessage implements Serializable {

    private ChatMessageType type; // 메시지 타입
    private Long chatRoomId; // 채팅방 Id
    private Long senderId; // 메시지 보낸 사람
    private String content; // 메시지
    private LocalDateTime createdAt; // 생성 시간

    /*
    * 환영 메시지 세팅 위함
    * 퇴장 메시지 세팅 위함
    * */
    public void setContent(String content) {
        this.content = content;
    }
}
