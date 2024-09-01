package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class CommentIdElement {

    private final Long commentId;

    private static CommentIdElementBuilder builder(Long commentId){
        return innerBuilder()
                .commentId(commentId);
    }

    public static CommentIdElement of(Comment comment) {
        return CommentIdElement.builder(
                    comment.getId()
                )
                .build();
    }
}
