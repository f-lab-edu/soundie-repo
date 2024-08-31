package com.soundie.comment.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostCommentCreateReqDto {
    private String content;

    public PostCommentCreateReqDto(String content){
        this.content = content;
    }
}
