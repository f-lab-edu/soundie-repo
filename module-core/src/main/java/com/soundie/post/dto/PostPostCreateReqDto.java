package com.soundie.post.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostPostCreateReqDto {
    private String title;
    private String musicPath;
    private String albumImgPath;
    private String albumName;

    @Builder
    private PostPostCreateReqDto(String title, String musicPath, String albumImgPath, String albumName){
        this.title = title;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
    }
}
