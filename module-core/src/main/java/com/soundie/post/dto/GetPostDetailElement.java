package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class GetPostDetailElement {

    private Long postId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private LocalDateTime createdAt;
    private Boolean liked;

    public static GetPostDetailElement of(Post post, Long postLikeCount, Long commentCount, Boolean liked) {
        return GetPostDetailElement.builder()
                .postId(post.getId())
                .title(post.getTitle())
                .artistName(post.getArtistName())
                .musicPath(post.getMusicPath())
                .albumImgPath(post.getAlbumImgPath())
                .albumName(post.getAlbumName())
                .likeCount(postLikeCount)
                .commentCount(commentCount)
                .createdAt(post.getCreatedAt())
                .liked(liked)
                .build();
    }
}
