package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class AuthorElement {

    private final Long memberId;
    private final String name;

    public static AuthorElement of(Member member){
        return new AuthorElement(
                member.getId(),
                member.getName()
        );
    }
}
