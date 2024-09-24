package com.soundie.comment.dto;

import com.soundie.comment.domain.CommentWithAuthor;
import com.soundie.member.dto.AuthorElement;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentElement {

    private Long commentId;
    private String content;
    private AuthorElement author;
    private LocalDateTime createdAt;

    public static GetCommentElement of(CommentWithAuthor commentWithAuthor){
        return new GetCommentElement(
                commentWithAuthor.getId(),
                commentWithAuthor.getContent(),
                AuthorElement.of(
                        commentWithAuthor.getMemberId(),
                        commentWithAuthor.getMemberName()
                ),
                commentWithAuthor.getCreatedAt()
        );
    }
}
