package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetPostDetailResDto {

    private final GetPostDetailElement post;

    private static GetPostDetailResDtoBuilder builder(GetPostDetailElement post){
        return innerBuilder()
                .post(post);
    }

    public static GetPostDetailResDto of(Post post, Long likeCount, Long commentCount, Boolean isLiked){
        return GetPostDetailResDto.builder(
                    GetPostDetailElement.of(post, likeCount, commentCount, isLiked)
                )
                .build();
    }
}
