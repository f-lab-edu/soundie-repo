package com.soundie.member.dto;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class AuthorElement {

    private final Long id;
    private final String name;

    public static AuthorElement of(Long memberId, String memberName) {
        return new AuthorElement(
                memberId,
                memberName
        );
    }
}
