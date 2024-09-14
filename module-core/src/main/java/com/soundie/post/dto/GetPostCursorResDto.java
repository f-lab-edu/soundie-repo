package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetPostCursorResDto {

    private final Collection<GetPostElement> posts;
    private final Long cursor;

    public static GetPostCursorResDto of(List<PostWithCount> postsWithCount, Integer size) {
        return new GetPostCursorResDto(
                postsWithCount.stream()
                        .map(GetPostElement::of)
                        .collect(Collectors.toList()),
                getNextCursor(postsWithCount, size)
        );
    }

    private static Long getNextCursor(List<PostWithCount> postsWithCount, Integer size) {
        Long nextCursor = null;
        if (postsWithCount.size() == size) {
            nextCursor = postsWithCount.get(size - 1).getId();
        }

        return nextCursor;
    }
}
