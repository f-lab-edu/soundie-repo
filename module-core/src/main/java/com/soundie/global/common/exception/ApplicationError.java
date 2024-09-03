package com.soundie.global.common.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ApplicationError {
    // 회원 관련
    MEMBER_NOT_FOUND(HttpStatus.BAD_REQUEST, "M001", "사용자를 찾을 수 없습니다."),
    INVALID_AUTHORITY(HttpStatus.BAD_REQUEST, "M002", "잘못된 권한입니다."),

    // 음원 게시물 관련
    POST_NOT_FOUND(HttpStatus.BAD_REQUEST, "P001", "음원 게시물을 찾을 수 없습니다."),

    // 댓글 관련
    
    // 채팅방 관련
    CHAT_ROOM_NOT_FOUND(HttpStatus.BAD_REQUEST, "R001", "채팅방을 찾을 수 없습니다."),
    DUPLICATE_CHAT_ROOM(HttpStatus.BAD_REQUEST, "R002", "해당 채팅방이 이미 존재합니다."),

    // 알림 관련

    // 서버 관련
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "S001", "서버 내부 에러가 발생했습니다.");

    private final HttpStatus status;
    private final String code;
    private final String message;
}
