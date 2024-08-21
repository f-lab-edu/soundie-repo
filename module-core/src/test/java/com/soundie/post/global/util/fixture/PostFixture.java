package com.soundie.post.global.util.fixture;

import com.soundie.post.domain.Post;

public class PostFixture {

    public static final Post createFirstMemberHavingFirstPost() {
        return new Post(
                MemberFixture.createFirstMember().getId(),
                "노래 제목1",
                MemberFixture.createFirstMember().getName(),
                "노래 주소1",
                "앨범 이미지 주소1",
                "앨범 이름1"
        );
    }

    public static final Post createFirstMemberHavingSecondPost() {
        return new Post(
                MemberFixture.createFirstMember().getId(),
                "노래 제목2",
                MemberFixture.createFirstMember().getName(),
                "노래 주소2",
                "앨범 이미지 주소2",
                "앨범 이름2"
        );
    }

    public static final Post createSecondMemberHavingFirstPost() {
        return new Post(
                MemberFixture.createSecondMember().getId(),
                "노래 제목",
                MemberFixture.createSecondMember().getName(),
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );
    }
}
