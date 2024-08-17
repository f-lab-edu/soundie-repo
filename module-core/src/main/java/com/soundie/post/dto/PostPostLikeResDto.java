package com.soundie.post.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class PostPostLikeResDto {

    private Number likeCount;
    private Boolean liked;

    public static PostPostLikeResDto of(Number likeCount, Boolean liked){
        return PostPostLikeResDto.builder()
                .likeCount(likeCount)
                .liked(liked)
                .build();
    }
}
