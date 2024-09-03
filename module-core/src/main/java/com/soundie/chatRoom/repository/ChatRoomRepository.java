package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;

import java.util.List;
import java.util.Optional;

public interface ChatRoomRepository {

    List<ChatRoom> findChatRooms();

    List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(Long memberId);

    Optional<ChatRoom> findChatRoomById(Long chatRoomId);

    ChatRoom save(ChatRoom chatRoom);

    void delete(ChatRoom chatRoom);
}
