package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostDetailElement {

    private final Long postId;
    private final String title;
    private final String artistName;
    private final String musicPath;
    private final String albumImgPath;
    private final String albumName;
    private final Number likeCount;
    private final Number commentCount;
    private final Boolean liked;
    private final LocalDateTime createdAt;

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
