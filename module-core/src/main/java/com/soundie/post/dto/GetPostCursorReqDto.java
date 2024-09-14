package com.soundie.post.dto;

import com.soundie.global.common.util.PaginationUtil;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostCursorReqDto {

    private Long cursor = PaginationUtil.START_CURSOR;
    private Integer size = PaginationUtil.POST_SIZE;
}
