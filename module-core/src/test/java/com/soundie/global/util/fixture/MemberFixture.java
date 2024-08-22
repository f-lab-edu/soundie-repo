package com.soundie.global.util.fixture;

import com.soundie.member.domain.Member;

public class MemberFixture {

    public static final Member createFirstMember(){
        Member member = new Member("Nick");
        member.setId(1L);
        return member;
    }

    public static final Member createSecondMember() {
        Member member = new Member("Nick");
        member.setId(2L);
        return member;
    }
}
