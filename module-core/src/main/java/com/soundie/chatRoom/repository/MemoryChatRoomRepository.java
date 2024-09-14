package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.member.domain.Member;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

public class MemoryChatRoomRepository implements ChatRoomRepository {
    private final Map<Long, ChatRoom> store = new ConcurrentHashMap<>();
    private AtomicLong sequence = new AtomicLong(0L);

    /*
     * 채팅방 목록 조회
     * */
    @Override
    public List<ChatRoom> findChatRooms() {
        return new ArrayList<>(store.values());
    }

    /*
    * 회원 Id로, 채팅방 목록 조회
    * */
    @Override
    public List<ChatRoom> findChatRoomsByHostMemberIdOrGuestMemberId(Long memberId){
        return findChatRooms().stream()
                .filter(cr -> cr.getHostMemberId().equals(memberId) || cr.getGuestMemberId().equals(memberId))
                .collect(Collectors.toList());
    }
    
    /*
     * 채팅방 Id로, 채팅방 조회
     * */
    @Override
    public Optional<ChatRoom> findChatRoomById(Long chatRoomId){
        return Optional.ofNullable(store.get(chatRoomId));
    }

    /*
     * Host 회원 Id + Guest 회원 Id로, 채팅방 조회
     * */
    @Override
    public Optional<ChatRoom> findChatRoomByHostMemberIdAndGuestMemberId(Long hostMemberId, Long guestMemberId) {
        return findChatRooms().stream()
                .filter(cr -> cr.getHostMemberId().equals(hostMemberId) && cr.getGuestMemberId().equals(guestMemberId))
                .findAny();

    }

    /*
     * 채팅방 저장
     * */
    @Override
    public ChatRoom save(ChatRoom chatRoom){
        chatRoom.setId(sequence.incrementAndGet());
        store.put(chatRoom.getId(), chatRoom);

        return chatRoom;
    }

    /*
     * 채팅방의 회원 Id 일치하면, null 로 수정(동적 쿼리)
     * */
    @Override
    public ChatRoom updateMemberNullIfMatchMember(ChatRoom chatRoom, Member member) {
        // 구현 필요
        return chatRoom;
    }

    /*
     * 채팅방 삭제
     * */
    @Override
    public void delete(ChatRoom chatRoom){
        store.remove(chatRoom.getId());
    }
}
