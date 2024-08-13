package com.soundie.post.domain;

import lombok.Data;

@Data
public class PostLike {

    private Long id;
    private Long memberId;
    private Long postId;

    public PostLike(Long memberId, Long postId){
        this.memberId = memberId;
        this.postId = postId;
    }
}
