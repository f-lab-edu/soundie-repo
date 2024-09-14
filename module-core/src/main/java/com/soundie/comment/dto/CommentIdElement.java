package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CommentIdElement {

    private final Long commentId;

    public static CommentIdElement of(Comment comment) {
        return new CommentIdElement(
                    comment.getId()
                );
    }
}
