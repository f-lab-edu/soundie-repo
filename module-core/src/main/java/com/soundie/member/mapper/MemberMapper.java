package com.soundie.member.mapper;

import com.soundie.member.domain.Member;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface MemberMapper {

    List<Member> findMembers();

    Optional<Member> findMemberById(@Param("id") Long id);

    void save(@Param("member") Member member);
}
