package com.soundie.chatMessage.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostChatMessageCreateReqDto {

    private String content; // 메시지 내용
    private int memberCnt; // 채팅방에 접속한 인원

    public PostChatMessageCreateReqDto(String content, int memberCnt){
        this.content = content;
        this.memberCnt = memberCnt;
    }
}
