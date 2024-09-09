package com.soundie.post.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostCursorReqDto {

    private Long cursor;
    private Integer size = 5;

    public GetPostCursorReqDto(Long cursor, Integer size){
        this.cursor = cursor;
        this.size = size;
    }
}
