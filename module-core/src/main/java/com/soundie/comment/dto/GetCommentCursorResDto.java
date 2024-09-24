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
public class GetCommentCursorResDto {

    private final Collection<GetCommentElement> comments;
    private final Long cursor;

    public static GetCommentCursorResDto of(List<CommentWithAuthor> commentsWithAuthor, Integer size) {
        return new GetCommentCursorResDto(
                commentsWithAuthor.stream()
                        .map(GetCommentElement::of)
                        .collect(Collectors.toList()),
                getNextCursor(commentsWithAuthor, size)
        );
    }

    private static Long getNextCursor(List<CommentWithAuthor> commentsWithAuthor, Integer size) {
        Long nextCursor = null;
        if (commentsWithAuthor.size() == size) {
            nextCursor = commentsWithAuthor.get(size - 1).getId();
        }

        return nextCursor;
    }
}
