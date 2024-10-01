package com.soundie.post.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PostLike {

    private Long id;
    private Long memberId;
    private Long postId;
    private LocalDateTime createdAt;

    public PostLike(Long memberId, Long postId){
        this.memberId = memberId;
        this.postId = postId;
    }
}
