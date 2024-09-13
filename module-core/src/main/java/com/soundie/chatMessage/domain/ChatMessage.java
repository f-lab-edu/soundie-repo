package com.soundie.chatMessage.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ChatMessage {

    private Long id; // 메시지 Id(Long)
    private Long chatRoomId; // 채팅방 Id(Long)
    private Long senderId; // 메시지 보낸 사람 Id(Long)
    private String typeName; // 메시지 타입 이름
    private String content; // 메시지 내용
    private LocalDateTime createdAt; // 메시지 생성 시간

    /*
     * 입장 메시지 위함
     * 퇴장 메시지 위함
     * */
    public ChatMessage(
            Long chatRoomId,
            Long senderId,
            ChatMessageType chatMessageType,
            String content,
            LocalDateTime createdAt){
        this.chatRoomId = chatRoomId;
        this.senderId = senderId;
        this.typeName = chatMessageType.getName();
        this.content = content;
        this.createdAt = createdAt;
    }
}
