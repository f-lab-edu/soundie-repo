package com.soundie.comment.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostCommentCreateReqDto {
    private String content;

    /*
    * test 를 위한 생성자
    * */
    public PostCommentCreateReqDto(String content){
        this.content = content;
    }
}
