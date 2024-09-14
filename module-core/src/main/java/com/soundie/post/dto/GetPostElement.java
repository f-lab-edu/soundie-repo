package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostElement {

    private final Long postId;
    private final String title;
    private final String artistName;
    private final String albumImgPath;
    private final String albumName;
    private final Number likeCount;
    private final Number commentCount;
    private final LocalDateTime createdAt;

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
