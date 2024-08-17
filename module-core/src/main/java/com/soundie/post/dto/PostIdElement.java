package com.soundie.post.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class PostIdElement {
    private Long postId;

    public static PostIdElement of(Long postId){
        return PostIdElement.builder()
                .postId(postId)
                .build();
    }
}
