package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetCommentCursorResDto {

    private final Collection<GetCommentElement> comments;
    private final Long cursor;

    private static GetCommentCursorResDtoBuilder builder(Collection<GetCommentElement> comments,
                                                         Long cursor) {
        return innerBuilder()
                .comments(comments)
                .cursor(cursor);
    }

    public static GetCommentCursorResDto of(List<Comment> comments, Map<Long, Member> linkedHashMap, Integer size) {
        return GetCommentCursorResDto.builder(
                        comments.stream()
                                .map(c -> {
                                    Member member = linkedHashMap.get(c.getId());
                                    return GetCommentElement.of(c, member);
                                })
                                .collect(Collectors.toList()),
                        getNextCursor(comments, size)
                )
                .build();
    }

    private static Long getNextCursor(List<Comment> comments, Integer size) {
        Long nextCursor = null;
        if (comments.size() == size) {
            nextCursor = comments.get(size - 1).getId();
        }

        return nextCursor;
    }
}
