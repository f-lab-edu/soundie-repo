package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MemberIdElement {

    private final Long memberId;

    public static MemberIdElement of(Member member){
        return new MemberIdElement(
                member.getId()
        );
    }
}
