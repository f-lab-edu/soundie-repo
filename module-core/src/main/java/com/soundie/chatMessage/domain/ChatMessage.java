package com.soundie.chatMessage.domain;

import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@NoArgsConstructor
public class ChatMessage implements Serializable {

    private String id; // 메시지 Id(String=UUID)
    private ChatMessageType type; // 메시지 타입
    private Long chatRoomId; // 채팅방 Id(Long)
    private Long senderId; // 메시지 보낸 사람 Id(Long)
    private String content; // 메시지 내용
    private LocalDateTime createdAt; // 메시지 생성 시간

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

    /*
    * 전송 메시지 위함
    * */
    public void setId(String id) {
        this.id = id;
    }
}
