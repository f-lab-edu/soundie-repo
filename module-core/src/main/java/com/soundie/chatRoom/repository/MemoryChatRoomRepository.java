package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

@Repository
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
     * 채팅방 저장
     * */
    @Override
    public ChatRoom save(ChatRoom chatRoom){
        chatRoom.setId(sequence.incrementAndGet());
        store.put(chatRoom.getId(), chatRoom);

        return chatRoom;
    }
}
