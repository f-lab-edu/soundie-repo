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
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Post {

    private Long id;
    private Long memberId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    public Post(Long memberId, String title, String artistName, String musicPath, String albumImgPath, String albumName) {
        this.memberId = memberId;
        this.title = title;
        this.artistName = artistName;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
    }

    /*
     * fixture 생성 위함
     * */
    public Post(Long id, Long memberId, String title, String artistName, String musicPath, String albumImgPath, String albumName) {
        this.id = id;
        this.memberId = memberId;
        this.title = title;
        this.artistName = artistName;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
        this.createdAt = LocalDateTime.now();
    }
}
