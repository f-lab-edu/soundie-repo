package com.soundie.post.dto;

import com.soundie.post.domain.PostWithCount;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetPostResDto {

    private final Collection<GetPostElement> posts;

    private static GetPostResDtoBuilder builder(Collection<GetPostElement> posts){
        return innerBuilder()
                .posts(posts);
    }

    public static GetPostResDto of(List<PostWithCount> postsWithCount) {
        return GetPostResDto.builder(
                    postsWithCount.stream()
                        .map(GetPostElement::of)
                        .collect(Collectors.toList())
                )
                .build();
    }
}
