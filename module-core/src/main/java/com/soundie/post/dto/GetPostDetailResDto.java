package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class GetPostDetailResDto {

    private GetPostDetailElement post;

    public static GetPostDetailResDto of(Post post, Long likeCount, Long commentCount, Boolean isLiked){
        return GetPostDetailResDto.builder()
                .post(GetPostDetailElement.of(post, likeCount, commentCount, isLiked))
                .build();
    }
}
