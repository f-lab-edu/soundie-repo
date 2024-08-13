package com.soundie.comment.dto;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder
public class GetCommentResDto {

    private Collection<GetCommentElement> comments;

    public static GetCommentResDto of(List<Comment> comments, Member member) {
        return GetCommentResDto.builder()
                .comments(comments.stream()
                        .map(c -> GetCommentElement.of(c, member))
                        .collect(Collectors.toList()))
                .build();
    }
}
