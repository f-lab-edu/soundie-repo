package com.soundie.post.global.util.fixture;

import com.soundie.member.domain.Member;

public class MemberFixture {

    public static final Member createFirstMember(){
        return new Member("Nick");
    }

    public static final Member createSecondMember() {
        return new Member("Alex");
    }
}
