package com.soundie.post.dto;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PostPostLikeResDto {

    private final Number likeCount;
    private final Boolean liked;

    public static PostPostLikeResDto of(Number likeCount, Boolean liked){
        return new PostPostLikeResDto(
                likeCount,
                liked
        );
    }
}
