package com.soundie.member.dto;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PostMemberCreateReqDto {
    private String name;

    public PostMemberCreateReqDto(String name){
        this.name = name;
    }
}
