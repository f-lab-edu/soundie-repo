package com.soundie.chatMessage.dto;

import com.soundie.global.common.util.PaginationUtil;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetChatMessageCursorReqDto {

    private Long cursor = PaginationUtil.START_CURSOR;
    private Integer size = PaginationUtil.CHAT_MESSAGE_SIZE;

    public GetChatMessageCursorReqDto(Long cursor, Integer size){
        this.cursor = cursor;
        this.size = size;
    }
}
