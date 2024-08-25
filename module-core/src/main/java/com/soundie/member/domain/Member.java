package com.soundie.member.domain;

import lombok.Data;

@Data
public class Member {

    private Long id;
    private String name;

    public Member(String name){
        this.name = name;
    }
}
