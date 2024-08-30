package com.soundie.post.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostPostCreateReqDto {
    private String title;
    private String musicPath;
    private String albumImgPath;
    private String albumName;

    public PostPostCreateReqDto(String title, String musicPath, String albumImgPath, String albumName){
        this.title = title;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
    }
}
