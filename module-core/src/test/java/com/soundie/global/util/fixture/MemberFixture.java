package com.soundie.global.util.fixture;

import com.soundie.member.domain.Member;

public class MemberFixture {

    public static final Member createFirstMember(){
        return new Member(
                1L,
                "Nick");
    }

    public static final Member createSecondMember() {
        return new Member(
                2L,
                "Nick");
    }
}
