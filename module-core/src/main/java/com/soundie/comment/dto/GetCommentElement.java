package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import com.soundie.member.dto.AuthorElement;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentElement {

    private Long commentId;
    private String content;
    private AuthorElement author;
    private LocalDateTime createdAt;

    public static GetCommentElement of(Comment comment, Member member){
        return new GetCommentElement(
                    comment.getId(),
                    comment.getContent(),
                    AuthorElement.of(member),
                    comment.getCreatedAt()
                );
    }
}
