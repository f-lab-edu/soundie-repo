package com.soundie.post.dto;

import com.soundie.global.common.util.PaginationConstant;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostCursorReqDto {

    private Long cursor = PaginationConstant.START_CURSOR;
    private Integer size = PaginationConstant.POST_SIZE;
}
