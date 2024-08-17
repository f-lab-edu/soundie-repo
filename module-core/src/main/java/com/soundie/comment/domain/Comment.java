package com.soundie.comment.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class Comment {

    private Long id;
    private Long memberId;
    private Long postId;
    private String content;
    private LocalDateTime createdAt;

    public Comment(Long memberId, Long postId, String content){
        this.memberId = memberId;
        this.postId = postId;
        this.content = content;
        this.createdAt = LocalDateTime.now();
    }
}
