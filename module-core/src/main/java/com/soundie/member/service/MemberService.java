package com.soundie.member.service;

import com.soundie.member.domain.Member;
import com.soundie.member.dto.MemberIdElement;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class MemberService {

    private final MemberRepository memberRepository;

    public MemberIdElement createMember() {
        Member member = new Member();
        Long memberId = memberRepository.save(member).getId();
        return new MemberIdElement(memberId);
    }
}
