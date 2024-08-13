package com.soundie.post.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class PostCommonLikeResDto {

    private Number likeCount;
    private Boolean liked;
}
