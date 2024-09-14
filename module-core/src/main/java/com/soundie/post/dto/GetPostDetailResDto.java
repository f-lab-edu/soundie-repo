package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostDetailResDto {

    private final GetPostDetailElement post;

    public static GetPostDetailResDto of(Post post, Number likeCount, Number commentCount, Boolean isLiked){
        return new GetPostDetailResDto(
                    GetPostDetailElement.of(post, likeCount, commentCount, isLiked)
                );
    }
}
