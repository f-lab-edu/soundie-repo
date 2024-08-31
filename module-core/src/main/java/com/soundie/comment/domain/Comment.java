package com.soundie.comment.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
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
    }

    /*
     * MemoryRepository 저장 위함
     * */
    public void setId(Long id) {
        this.id = id;
    }
}
