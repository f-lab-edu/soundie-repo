package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import com.soundie.member.dto.AuthorElement;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetCommentElement {

    private final Long commentId;
    private final String content;
    private final LocalDateTime createdAt;
    private final AuthorElement author;

    private static GetCommentElementBuilder builder(Long commentId, String content, LocalDateTime createdAt, Member member){
        return innerBuilder()
                .commentId(commentId)
                .content(content)
                .createdAt(createdAt)
                .author(AuthorElement.of(member));
    }

    public static GetCommentElement of(Comment comment, Member member){
        return GetCommentElement.builder(
                    comment.getId(),
                    comment.getContent(),
                    comment.getCreatedAt(),
                    member
                )
                .build();
    }
}
