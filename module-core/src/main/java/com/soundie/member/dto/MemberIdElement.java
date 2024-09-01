package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class MemberIdElement {

    private final Long memberId;

    private static MemberIdElementBuilder builder(Long memberId){
        return innerBuilder()
                .memberId(memberId);
    }

    public static MemberIdElement of(Member member){
        return MemberIdElement.builder(
                    member.getId()
                )
                .build();
    }
}
