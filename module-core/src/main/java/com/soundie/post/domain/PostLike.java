package com.soundie.post.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class PostLike {

    private Long id;
    private Long memberId;
    private Long postId;
    private LocalDateTime createdAt;

    public PostLike(Long memberId, Long postId){
        this.memberId = memberId;
        this.postId = postId;
        this.createdAt = LocalDateTime.now();
    }
}
