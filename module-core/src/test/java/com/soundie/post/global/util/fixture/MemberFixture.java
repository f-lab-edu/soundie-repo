package com.soundie.post.global.util.fixture;

import com.soundie.member.domain.Member;

public class MemberFixture {
    public Member createMember() {
        Member member = new Member("Alex");
        member.setId(1L);
        return member;
    }
}
