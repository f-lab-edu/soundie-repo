package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetPostDetailElement {

    private final Long postId;
    private final String title;
    private final String artistName;
    private final String musicPath;
    private final String albumImgPath;
    private final String albumName;
    private final Number likeCount;
    private final Number commentCount;
    private final LocalDateTime createdAt;
    private final Boolean liked;

    private static GetPostDetailElementBuilder builder(
            Long postId,
            String title,
            String artistName,
            String musicPath,
            String albumImgPath,
            String albumName,
            Number likeCount,
            Number commentCount,
            LocalDateTime createdAt,
            Boolean liked) {
        return innerBuilder()
                .postId(postId)
                .title(title)
                .artistName(artistName)
                .musicPath(musicPath)
                .albumImgPath(albumImgPath)
                .albumName(albumName)
                .likeCount(likeCount)
                .commentCount(commentCount)
                .createdAt(createdAt)
                .liked(liked);
    }

    public static GetPostDetailElement of(Post post, Long postLikeCount, Long commentCount, Boolean liked) {
        return GetPostDetailElement.builder(
                    post.getId(),
                    post.getTitle(),
                    post.getArtistName(),
                    post.getMusicPath(),
                    post.getAlbumImgPath(),
                    post.getAlbumName(),
                    postLikeCount,
                    commentCount,
                    post.getCreatedAt(),
                    liked
                )
                .build();
    }
}
