package com.soundie.global.util.fixture;

import com.soundie.post.domain.Post;

public class PostFixture {

    public static final Post createFirstMemberHavingFirstPost() {
        Post post = new Post(
                MemberFixture.createFirstMember().getId(),
                "노래 제목1",
                MemberFixture.createFirstMember().getName(),
                "노래 주소1",
                "앨범 이미지 주소1",
                "앨범 이름1"
        );
        post.setId(1L);
        return post;
    }

    public static final Post createFirstMemberHavingSecondPost() {
        Post post = new Post(
                MemberFixture.createFirstMember().getId(),
                "노래 제목2",
                MemberFixture.createFirstMember().getName(),
                "노래 주소2",
                "앨범 이미지 주소2",
                "앨범 이름2"
        );
        post.setId(2L);
        return post;
    }

    public static final Post createSecondMemberHavingFirstPost() {
        Post post = new Post(
                MemberFixture.createSecondMember().getId(),
                "노래 제목",
                MemberFixture.createSecondMember().getName(),
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );
        post.setId(1L);
        return post;
    }
}
