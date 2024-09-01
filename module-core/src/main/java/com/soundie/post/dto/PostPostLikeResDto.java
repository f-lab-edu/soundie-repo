package com.soundie.post.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class PostPostLikeResDto {

    private final Number likeCount;
    private final Boolean liked;

    private static PostPostLikeResDtoBuilder builder(Number likeCount, Boolean liked){
        return innerBuilder()
                .likeCount(likeCount)
                .liked(liked);
    }

    public static PostPostLikeResDto of(Number likeCount, Boolean liked){
        return PostPostLikeResDto.builder(
                    likeCount,
                    liked
                )
                .build();
    }
}
