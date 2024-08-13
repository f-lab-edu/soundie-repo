package com.soundie.member.repository;

import com.soundie.member.domain.Member;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Repository
public class MemberRepository {

    private static final Map<Long, Member> store = new HashMap<>(); //static
    private static long sequence = 0L; //static

    /*
     * 회원 목록 조회
     * */
    public List<Member> findMembers() {
        return new ArrayList<>(store.values());
    }

    /*
    * 회원 Id로, 회원 조회
    * */
    public Member findMemberById(Long memberId) {
        return store.get(memberId);
    }

    /*
     * 회원 저장
     * */
    public Member save(Member member){
        member.setId(++sequence);
        store.put(member.getId(), member);

        return member;
    }
}