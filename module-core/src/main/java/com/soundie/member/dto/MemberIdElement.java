package com.soundie.member.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class MemberIdElement {

    private Long memberId;

    public static MemberIdElement of(Long memberId){
        return MemberIdElement.builder()
                .memberId(memberId)
                .build();
    }
}
