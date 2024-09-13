package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.*;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class AuthorElement {

    private Long memberId;
    private String name;

    public static AuthorElement of(Member member){
        return new AuthorElement(
                    member.getId(),
                    member.getName()
                );
    }
}
