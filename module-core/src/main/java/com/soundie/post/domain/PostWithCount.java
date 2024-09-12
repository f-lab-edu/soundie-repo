package com.soundie.post.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostWithCount {

    private Long id;
    private Long memberId;
    private String title;
    private String artistName;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    public PostWithCount(
            Long id,
            Long memberId,
            String title,
            String artistName,
            String albumImgPath,
            String albumName,
            Number likeCount,
            Number commentCount,
            LocalDateTime createdAt
    ){
        this.id = id;
        this.memberId = memberId;
        this.title = title;
        this.artistName = artistName;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
        this.likeCount = likeCount;
        this.commentCount = commentCount;
        this.createdAt = createdAt;
    }
}
