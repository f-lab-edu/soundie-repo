package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;


import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentCursorResDto {

    private final Collection<GetCommentElement> comments;
    private final Long cursor;

    public static GetCommentCursorResDto of(List<Comment> comments, Map<Long, Member> commentsByMember, Integer size) {
        return new GetCommentCursorResDto(
                comments.stream()
                        .map(c -> {
                            Member member = commentsByMember.get(c.getId());
                            return GetCommentElement.of(c, member);
                        })
                        .collect(Collectors.toList()),
                getNextCursor(comments, size)
        );
    }

    private static Long getNextCursor(List<Comment> comments, Integer size) {
        Long nextCursor = null;
        if (comments.size() == size) {
            nextCursor = comments.get(size - 1).getId();
        }

        return nextCursor;
    }
}
