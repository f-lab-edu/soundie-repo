package com.soundie.member.service;

import com.soundie.member.domain.Member;
import com.soundie.member.dto.MemberIdElement;
import com.soundie.member.repository.MemoryMemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class MemberService {

    private final MemoryMemberRepository memberRepository;

    public MemberIdElement createMember() {
        Member member = new Member("정원석");

        Long memberId = memberRepository.save(member).getId();
        return MemberIdElement.of(memberId);
    }
}
