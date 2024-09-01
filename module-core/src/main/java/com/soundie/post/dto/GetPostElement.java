package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetPostElement {

    private final Long postId;
    private final String title;
    private final String artistName;
    private final String musicPath;
    private final String albumImgPath;
    private final String albumName;
    private final Number likeCount;
    private final Number commentCount;
    private final LocalDateTime createdAt;

    private static GetPostElementBuilder builder(
            Long postId,
            String title,
            String artistName,
            String musicPath,
            String albumImgPath,
            String albumName,
            Number likeCount,
            Number commentCount,
            LocalDateTime createdAt) {
        return innerBuilder()
                .postId(postId)
                .title(title)
                .artistName(artistName)
                .musicPath(musicPath)
                .albumImgPath(albumImgPath)
                .albumName(albumName)
                .likeCount(likeCount)
                .commentCount(commentCount)
                .createdAt(createdAt);
    }

    public static GetPostElement of(PostWithCount postWithCount){
        return GetPostElement.builder(
                    postWithCount.getId(),
                    postWithCount.getTitle(),
                    postWithCount.getArtistName(),
                    postWithCount.getMusicPath(),
                    postWithCount.getAlbumImgPath(),
                    postWithCount.getAlbumName(),
                    postWithCount.getLikeCount(),
                    postWithCount.getCommentCount(),
                    postWithCount.getCreatedAt()
                )
                .build();
    }
}
