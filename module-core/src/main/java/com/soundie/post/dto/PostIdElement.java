package com.soundie.post.dto;

import lombok.Getter;

@Getter
public class PostIdElement {
    private final Long postId;

    public PostIdElement(Long postId){
        this.postId = postId;
    }
}
