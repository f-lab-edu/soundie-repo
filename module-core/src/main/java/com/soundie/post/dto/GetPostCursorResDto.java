package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetPostCursorResDto {

    private final Collection<GetPostElement> posts;
    private final Long cursor;

    private static GetPostCursorResDtoBuilder builder(Collection<GetPostElement> posts,
                                                      Long cursor){
        return innerBuilder()
                .posts(posts)
                .cursor(cursor);
    }

    public static GetPostCursorResDto of(List<PostWithCount> postsWithCount, Integer size) {
        return GetPostCursorResDto.builder(
                        postsWithCount.stream()
                                .map(GetPostElement::of)
                                .collect(Collectors.toList()),
                        getNextCursor(postsWithCount, size)
                )
                .build();
    }

    private static Long getNextCursor(List<PostWithCount> postsWithCount, Integer size) {
        Long nextCursor = null;
        if (postsWithCount.size() == size) {
            nextCursor = postsWithCount.get(size - 1).getId();
        }

        return nextCursor;
    }
}
