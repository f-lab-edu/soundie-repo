package com.soundie.member.dto;

import lombok.Getter;

@Getter
public class MemberIdElement {

    private final Long memberId;

    public MemberIdElement(Long memberId){
        this.memberId = memberId;
    }
}
