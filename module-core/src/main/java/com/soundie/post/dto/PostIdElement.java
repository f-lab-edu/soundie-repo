package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class PostIdElement {

    private final Long postId;

    private static PostIdElementBuilder builder(Long postId){
        return innerBuilder()
                .postId(postId);
    }

    public static PostIdElement of(Post post){
        return PostIdElement.builder(
                    post.getId()
                )
                .build();
    }
}
