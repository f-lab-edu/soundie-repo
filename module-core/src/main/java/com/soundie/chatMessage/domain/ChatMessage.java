package com.soundie.chatMessage.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@NoArgsConstructor
public class ChatMessage implements Serializable {

    private ChatMessageType type; // 메시지 타입
    private Long chatRoomId; // 채팅방 Id
    private Long senderId; // 메시지 보낸 사람
    private String content; // 메시지
    private LocalDateTime createdAt; // 생성 시간

    /*
    * 입장 메시지 위함
    * 퇴장 메시지 위함
    * */
    public ChatMessage(
            ChatMessageType type,
            Long chatRoomId,
            String content,
            LocalDateTime createdAt){
        this.type = type;
        this.chatRoomId = chatRoomId;
        this.content = content;
        this.createdAt = createdAt;
    }
}
