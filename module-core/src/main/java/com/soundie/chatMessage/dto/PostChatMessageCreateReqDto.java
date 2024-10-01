package com.soundie.chatMessage.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostChatMessageCreateReqDto {

    private String content; // 메시지 내용
    private String contentImgPath; // 메시지 이미지 주소
    private int memberCnt; // 채팅방에 접속한 인원
}
