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
public class GetCommentResDto {

    private final Collection<GetCommentElement> comments;

    private static GetCommentResDtoBuilder builder(Collection<GetCommentElement> comments) {
        return innerBuilder()
                .comments(comments);
    }

    public static GetCommentResDto of(List<Comment> comments, Map<Long, Member> commentsByMember) {
        return GetCommentResDto.builder(
                    comments.stream()
                            .map(c -> {
                                Member member = commentsByMember.get(c.getId());
                                return GetCommentElement.of(c, member);
                            })
                            .collect(Collectors.toList())
                )
                .build();
    }
}
