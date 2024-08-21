package com.soundie.post.global.util.fixture;

import com.soundie.post.domain.Post;

public class PostFixture {

    private static final MemberFixture memberFixture = new MemberFixture();
    
    public static final Post POST_FIXTURE1 = new Post(
            memberFixture.createMember().getId(), 
            "노래 제목",
            memberFixture.createMember().getName(),
            "노래 주소",
            "앨범 이미지 주소",
            "앨범 이름"
    );
}
