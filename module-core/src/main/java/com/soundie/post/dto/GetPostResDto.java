package com.soundie.post.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class GetPostResDto {

    private Long postId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private String createdAt;
    private Boolean liked;
}
