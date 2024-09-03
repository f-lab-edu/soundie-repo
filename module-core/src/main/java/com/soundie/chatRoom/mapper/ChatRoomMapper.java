package com.soundie.chatRoom.mapper;

import com.soundie.chatRoom.domain.ChatRoom;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ChatRoomMapper {

    List<ChatRoom> findChatRooms();

    List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(@Param("memberId") Long memberId);

    Optional<ChatRoom> findChatRoomById(@Param("id") Long id);

    void save(@Param("chatRoom") ChatRoom chatRoom);

    void delete(@Param("chatRoom") ChatRoom chatRoom);
}
