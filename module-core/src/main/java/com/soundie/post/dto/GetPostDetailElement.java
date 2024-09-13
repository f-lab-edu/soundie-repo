package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostDetailElement {

    private Long postId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private Boolean liked;
    private LocalDateTime createdAt;

    public static GetPostDetailElement of(Post post, Number postLikeCount, Number commentCount, Boolean liked) {
        return new GetPostDetailElement(
                    post.getId(),
                    post.getTitle(),
                    post.getArtistName(),
                    post.getMusicPath(),
                    post.getAlbumImgPath(),
                    post.getAlbumName(),
                    postLikeCount,
                    commentCount,
                    liked,
                    post.getCreatedAt()
                );
    }
}
