package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.*;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostDetailResDto {

    private GetPostDetailElement post;

    public static GetPostDetailResDto of(Post post, Long likeCount, Long commentCount, Boolean isLiked){
        return new GetPostDetailResDto(
                    GetPostDetailElement.of(post, likeCount, commentCount, isLiked)
                );
    }
}
