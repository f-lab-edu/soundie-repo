package com.soundie.post.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PostPostCreateReqDto {
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
}
