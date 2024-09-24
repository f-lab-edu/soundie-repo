package com.soundie.comment.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommentWithAuthor {

    private Long id;
    private Long memberId;
    private String memberName;
    private Long postId;
    private String content;
    private LocalDateTime createdAt;
}
