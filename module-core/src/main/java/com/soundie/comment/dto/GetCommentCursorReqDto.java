package com.soundie.comment.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentCursorReqDto {

    private Long cursor;
    private Integer size = 10;

    public GetCommentCursorReqDto(Long cursor, Integer size){
        this.cursor = cursor;
        this.size = size;
    }
}