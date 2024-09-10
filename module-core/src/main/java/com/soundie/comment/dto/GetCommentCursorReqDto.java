package com.soundie.comment.dto;

import com.soundie.global.common.util.PaginationUtil;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentCursorReqDto {

    private Long cursor = PaginationUtil.START_CURSOR;
    private Integer size = PaginationUtil.COMMENT_SIZE;

    public GetCommentCursorReqDto(Long cursor, Integer size){
        this.cursor = cursor;
        this.size = size;
    }
}