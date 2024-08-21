package com.soundie.comment.dto;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostCommentCreateReqDto {
    private String content;

    @Builder
    private PostCommentCreateReqDto(String content){
        this.content = content;
    }
}
