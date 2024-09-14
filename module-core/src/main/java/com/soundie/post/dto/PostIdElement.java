package com.soundie.post.dto;

import com.soundie.post.domain.Post;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PostIdElement {

    private final Long postId;

    public static PostIdElement of(Post post){
        return new PostIdElement(
                    post.getId()
                );
    }
}
