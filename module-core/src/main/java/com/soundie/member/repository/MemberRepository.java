package com.soundie.member.repository;

import com.soundie.member.domain.Member;

import java.util.List;
import java.util.Optional;

public interface MemberRepository {

    List<Member> findMembers();

    Optional<Member> findMemberById(Long memberId);

    Member save(Member member);
}
