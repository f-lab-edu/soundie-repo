package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class AuthorElement {

    private final Long memberId;
    private final String name;

    private static AuthorElementBuilder builder(Long memberId, String name){
        return innerBuilder()
                .memberId(memberId)
                .name(name);
    }

    public static AuthorElement of(Member member){
        return AuthorElement.builder(
                    member.getId(),
                    member.getName()
                )
                .build();
    }
}
