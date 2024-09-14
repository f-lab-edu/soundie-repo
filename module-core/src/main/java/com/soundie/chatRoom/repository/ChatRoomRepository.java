package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.member.domain.Member;

import java.util.List;
import java.util.Optional;

public interface ChatRoomRepository {

    List<ChatRoom> findChatRooms();

    List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(Long memberId);

    Optional<ChatRoom> findChatRoomById(Long chatRoomId);

    Optional<ChatRoom> findChatRoomByHostMemberIdAndGuestMemberId(Long hostMemberId, Long guestMemberId);

    ChatRoom save(ChatRoom chatRoom);

    ChatRoom updateMemberNullIfMatchMember(ChatRoom chatRoom, Member member);

    void delete(ChatRoom chatRoom);
}
