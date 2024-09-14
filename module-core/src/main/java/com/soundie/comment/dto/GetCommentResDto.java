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
public class GetCommentResDto {

    private final Collection<GetCommentElement> comments;

    public static GetCommentResDto of(List<Comment> comments, Map<Long, Member> commentsByMember) {
        return new GetCommentResDto(
                comments.stream()
                        .map(c -> {
                            Member member = commentsByMember.get(c.getId());
                            return GetCommentElement.of(c, member);
                        })
                        .collect(Collectors.toList())
        );
    }
}
