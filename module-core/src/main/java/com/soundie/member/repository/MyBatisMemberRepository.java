package com.soundie.member.repository;

import com.soundie.member.domain.Member;
import com.soundie.member.mapper.MemberMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisMemberRepository implements MemberRepository {

    private final MemberMapper memberMapper;

    @Override
    public List<Member> findMembers() {
        return memberMapper.findMembers();
    }

    @Override
    public Optional<Member> findMemberById(Long memberId) {
        return memberMapper.findMemberById(memberId);
    }

    @Override
    public Member save(Member member) {
        memberMapper.save(member);
        return member;
    }
}
