package com.soundie.member.domain;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Member {

    private Long id;
    private String name;
    private LocalDateTime createdAt;

    public Member(String name){
        this.name = name;
    }

    /*
    * MemoryRepository 저장 위함
    * */
    public void setId(Long id) {
        this.id = id;
    }
}
