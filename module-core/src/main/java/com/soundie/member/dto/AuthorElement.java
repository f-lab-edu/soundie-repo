package com.soundie.member.dto;

import com.soundie.member.domain.Member;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public class AuthorElement {

    private Long memberId;
    private String name;

    public AuthorElement(Member member){
        this.memberId = member.getId();
        this.name = member.getName();
    }
}
