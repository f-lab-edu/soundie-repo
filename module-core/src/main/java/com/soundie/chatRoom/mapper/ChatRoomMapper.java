package com.soundie.chatRoom.mapper;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.member.domain.Member;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ChatRoomMapper {

    List<ChatRoom> findChatRooms();

    List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(@Param("memberId") Long memberId);

    Optional<ChatRoom> findChatRoomById(@Param("id") Long id);

    Optional<ChatRoom> findChatRoomByHostMemberIdAndGuestMemberId(
            @Param("hostMemberId") Long hostMemberId,
            @Param("guestMemberId") Long guestMemberId);

    void save(@Param("chatRoom") ChatRoom chatRoom);

    void updateMemberNullIfMatchMember(
            @Param("chatRoom") ChatRoom chatRoom,
            @Param("member") Member member);

    void delete(@Param("chatRoom") ChatRoom chatRoom);
}
