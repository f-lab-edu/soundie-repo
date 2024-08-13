package com.soundie.post.dto;

import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class GetPostDetailResDto {

    private GetPostDetailElement post;

    public static GetPostDetailResDto of(Post post, Member member){
        return GetPostDetailResDto.builder()
                .post(GetPostDetailElement.of(post, member))
                .build();
    }
}
