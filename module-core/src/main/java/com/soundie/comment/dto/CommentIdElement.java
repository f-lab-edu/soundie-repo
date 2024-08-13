package com.soundie.comment.dto;

import lombok.Getter;

@Getter
public class CommentIdElement {
    private final Long commentId;

    public CommentIdElement(Long commentId) {
        this.commentId = commentId;
    }
}
