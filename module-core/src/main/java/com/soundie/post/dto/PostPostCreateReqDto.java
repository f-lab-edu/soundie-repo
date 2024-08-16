package com.soundie.post.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostPostCreateReqDto {
    private String title;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
}
