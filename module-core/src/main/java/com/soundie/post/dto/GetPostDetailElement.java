package com.soundie.post.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
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

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    public static GetPostDetailElement of(Post post, Long postLikeCount, Long commentCount, Boolean liked) {
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
