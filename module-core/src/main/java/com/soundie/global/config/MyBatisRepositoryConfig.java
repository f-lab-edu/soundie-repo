package com.soundie.global.config;

import com.soundie.member.mapper.MemberMapper;
import com.soundie.member.repository.MemberRepository;
import com.soundie.member.repository.MyBatisMemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class MyBatisRepositoryConfig {

    private final MemberMapper memberMapper;

    @Bean
    public MemberRepository memberRepository(){
        return new MyBatisMemberRepository(memberMapper);
    }
}
