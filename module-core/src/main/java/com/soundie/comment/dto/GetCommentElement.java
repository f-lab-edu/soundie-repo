package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import com.soundie.member.dto.AuthorElement;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class GetCommentElement {

    private Long commentId;
    private String content;
    private LocalDateTime createdAt;
    private AuthorElement author;

    public static GetCommentElement of(Comment comment, Member member){
        return GetCommentElement.builder()
                .commentId(comment.getId())
                .content(comment.getContent())
                .createdAt(comment.getCreatedAt())
                .author(AuthorElement.of(member))
                .build();
    }
}
