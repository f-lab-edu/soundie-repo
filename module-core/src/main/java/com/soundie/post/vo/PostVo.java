package com.soundie.post.vo;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor
public class PostVo {

    private Long id;
    private Long memberId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private LocalDateTime createdAt;
}
