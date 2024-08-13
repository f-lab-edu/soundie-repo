package com.soundie.post.domain;

import com.soundie.member.domain.Member;
import lombok.Data;

@Data
public class PostLike {

    private Long id;
    private Post post;
    private Member member;
}
