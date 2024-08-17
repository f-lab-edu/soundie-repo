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
        member.setName("정원석");

        Long memberId = memberRepository.save(member).getId();
        return MemberIdElement.of(memberId);
    }
}
