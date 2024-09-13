package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostElement {

    private Long postId;
    private String title;
    private String artistName;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private LocalDateTime createdAt;

    public static GetPostElement of(PostWithCount postWithCount){
        return new GetPostElement(
                postWithCount.getId(),
                postWithCount.getTitle(),
                postWithCount.getArtistName(),
                postWithCount.getAlbumImgPath(),
                postWithCount.getAlbumName(),
                postWithCount.getLikeCount(),
                postWithCount.getCommentCount(),
                postWithCount.getCreatedAt()
        );
    }
}
