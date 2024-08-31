package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class GetPostElement {

    private Long postId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private LocalDateTime createdAt;

    public static GetPostElement of(PostWithCount postWithCount){
        return GetPostElement.builder()
                .postId(postWithCount.getId())
                .title(postWithCount.getTitle())
                .artistName(postWithCount.getArtistName())
                .musicPath(postWithCount.getMusicPath())
                .albumImgPath(postWithCount.getAlbumImgPath())
                .albumName(postWithCount.getAlbumName())
                .likeCount(postWithCount.getLikeCount())
                .commentCount(postWithCount.getCommentCount())
                .createdAt(postWithCount.getCreatedAt())
                .build();
    }
}
