package com.soundie.comment.dto;

import com.soundie.comment.domain.CommentWithAuthor;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentResDto {

    private final Collection<GetCommentElement> comments;

    public static GetCommentResDto of(List<CommentWithAuthor> commentsWithAuthor) {
        return new GetCommentResDto(
                commentsWithAuthor.stream()
                        .map(GetCommentElement::of)
                        .collect(Collectors.toList())
        );
    }
}
