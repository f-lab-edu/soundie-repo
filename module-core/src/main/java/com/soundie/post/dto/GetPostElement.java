package com.soundie.post.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
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
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    public static GetPostElement of(PostWithCount postWithCount){
        return new GetPostElement(
                postWithCount.getId(),
                postWithCount.getTitle(),
                postWithCount.getArtistName(),
                postWithCount.getMusicPath(),
                postWithCount.getAlbumImgPath(),
                postWithCount.getAlbumName(),
                postWithCount.getLikeCount(),
                postWithCount.getCommentCount(),
                postWithCount.getCreatedAt()
        );
    }
}
