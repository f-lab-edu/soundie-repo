package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder
public class GetPostResDto {

    private Collection<GetPostElement> posts;

    public static GetPostResDto of(List<Post> posts) {
        return GetPostResDto.builder()
                .posts(posts.stream()
                        .map(p -> GetPostElement.of(p))
                        .collect(Collectors.toList()))
                .build();
    }
}
