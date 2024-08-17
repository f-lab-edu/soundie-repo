package com.soundie.post.dto;

import com.soundie.post.domain.Post;
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

    public static GetPostElement of(Post post){
        return GetPostElement.builder()
                .postId(post.getId())
                .title(post.getTitle())
                .artistName(post.getArtistName())
                .musicPath(post.getMusicPath())
                .albumImgPath(post.getAlbumImgPath())
                .albumName(post.getAlbumName())
                .likeCount(post.getLikes().size())
                .commentCount(post.getComments().size())
                .createdAt(post.getCreatedAt())
                .build();
    }
}
