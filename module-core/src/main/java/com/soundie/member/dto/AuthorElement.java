package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class AuthorElement {

    private Long memberId;
    private String name;

    public static AuthorElement of(Member member){
        return AuthorElement.builder()
                .memberId(member.getId())
                .name(member.getName())
                .build();
    }
}
