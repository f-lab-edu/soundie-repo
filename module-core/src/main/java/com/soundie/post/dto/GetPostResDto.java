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
public class GetPostResDto {

    private final Collection<GetPostElement> posts;

    public static GetPostResDto of(List<PostWithCount> postsWithCount) {
        return new GetPostResDto(
                    postsWithCount.stream()
                            .map(GetPostElement::of)
                            .collect(Collectors.toList())
                );
    }
}
