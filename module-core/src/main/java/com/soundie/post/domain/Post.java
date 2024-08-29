package com.soundie.post.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Post {

    private Long id;
    private Long memberId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
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
     * MemoryRepository 저장 위함
     * */
    public void setId(Long id) {
        this.id = id;
    }
}
