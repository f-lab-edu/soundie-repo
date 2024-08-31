package com.soundie.member.repository;

import com.soundie.member.domain.Member;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class MemoryMemberRepository implements MemberRepository {

    private final Map<Long, Member> store = new ConcurrentHashMap<>();
    private AtomicLong sequence = new AtomicLong(0L);

    /*
     * 회원 목록 조회
     * */
    @Override
    public List<Member> findMembers() {
        return new ArrayList<>(store.values());
    }

    /*
    * 회원 Id로, 회원 조회
    * */
    @Override
    public Optional<Member> findMemberById(Long memberId) {
        return Optional.ofNullable(store.get(memberId));
    }

    /*
     * 회원 저장
     * */
    @Override
    public Member save(Member member){
        member.setId(sequence.incrementAndGet());
        store.put(member.getId(), member);

        return member;
    }

    public void clearStore() {
        store.clear();
    }
}
