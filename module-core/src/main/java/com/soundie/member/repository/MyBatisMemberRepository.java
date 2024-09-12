package com.soundie.member.repository;

import com.soundie.global.common.util.CacheNames;
import com.soundie.member.domain.Member;
import com.soundie.member.mapper.MemberMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisMemberRepository implements MemberRepository {

    private final MemberMapper memberMapper;

    /*
     * 회원 목록 조회
     * */
    @Override
    public List<Member> findMembers() {
        return memberMapper.findMembers();
    }

    /*
     * 회원 Id로, 회원 조회
     * */
    @Override
    @Cacheable(cacheNames = CacheNames.MEMBER, key = "'memberId_' + #memberId")
    public Optional<Member> findMemberById(Long memberId) {
        return memberMapper.findMemberById(memberId);
    }

    /*
     * 회원 저장
     * */
    @Override
    public Member save(Member member) {
        memberMapper.save(member);
        return member;
    }
}
