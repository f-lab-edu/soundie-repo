package com.soundie.member.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class Member {

    private Long id;
    private String name;
    private LocalDateTime createdAt;

    public Member(String name){
        this.name = name;
    }
}
