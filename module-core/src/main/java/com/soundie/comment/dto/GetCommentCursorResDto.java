package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import lombok.*;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetCommentCursorResDto {

    private Collection<GetCommentElement> comments;
    private Long cursor;

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
