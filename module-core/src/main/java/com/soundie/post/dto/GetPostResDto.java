package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder
public class GetPostResDto {

    private Collection<GetPostElement> posts;

    public static GetPostResDto of(List<PostWithCount> postsWithCount) {
        return GetPostResDto.builder()
                .posts(postsWithCount.stream()
                        .map(pwc -> GetPostElement.of(pwc))
                        .collect(Collectors.toList()))
                .build();
    }
}
