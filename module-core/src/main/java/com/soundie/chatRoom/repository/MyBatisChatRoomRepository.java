package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.chatRoom.mapper.ChatRoomMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisChatRoomRepository implements ChatRoomRepository {

    private final ChatRoomMapper chatRoomMapper;

    /*
     * 채팅방 목록 조회
     * */
    @Override
    public List<ChatRoom> findChatRooms() {
        return chatRoomMapper.findChatRooms();
    }

    /*
     * 회원 Id로, 채팅방 목록 조회
     * */
    @Override
    public List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(Long memberId) {
        return chatRoomMapper.findChatRoomsByHostMemberIdOrGuestMemberId(memberId);
    }

    /*
     * 채팅방 Id로, 채팅방 조회
     * */
    @Override
    public Optional<ChatRoom> findChatRoomById(Long chatRoomId) {
        return chatRoomMapper.findChatRoomById(chatRoomId);
    }

    /*
    * Host 회원 Id + Guest 회원 Id로, 채팅방 조회
    * */
    @Override
    public Optional<ChatRoom> findChatRoomByHostMemberIdAndGuestMemberId(Long hostMemberId, Long guestMemberId) {
        return chatRoomMapper.findChatRoomByHostMemberIdAndGuestMemberId(hostMemberId, guestMemberId);
    }

    /*
     * 채팅방 저장
     * */
    @Override
    public ChatRoom save(ChatRoom chatRoom) {
        chatRoomMapper.save(chatRoom);
        return chatRoom;
    }

    /*
     * 채팅방 삭제
     * */
    @Override
    public void delete(ChatRoom chatRoom) {
        chatRoomMapper.delete(chatRoom);
    }
}
