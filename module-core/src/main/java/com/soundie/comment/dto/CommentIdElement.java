package com.soundie.comment.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class CommentIdElement {
    private final Long commentId;

    public static CommentIdElement of(Long commentId) {
        return CommentIdElement.builder()
                .commentId(commentId)
                .build();
    }
}
