package com.soundie.chatRoom.repository;

import com.soundie.chatRoom.domain.ChatRoom;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Repository
public class ChatRoomRepository {
    private static final Map<Long, ChatRoom> store = new HashMap<>(); //static
    private static long sequence = 0L; //static

    /*
     * 채팅방 목록 조회
     * */
    public List<ChatRoom> findChatRooms() {
        return new ArrayList<>(store.values());
    }

    /*
    * 회원 Id로, 채팅방 목록 조회
    * */
    public List<ChatRoom> findChatRoomsByMemberId(Long memberId){
        return findChatRooms().stream()
                .filter(cr -> cr.getMemberId().equals(memberId))
                .collect(Collectors.toList());
    }
    
    
    /*
     * 채팅방 Id로, 채팅방 조회
     * */
    public ChatRoom findChatRoomById(Long chatRoomId){
        return store.get(chatRoomId);
    }

    /*
     * 채팅방 저장
     * */
    public ChatRoom save(ChatRoom chatRoom){
        chatRoom.setId(++sequence);
        store.put(chatRoom.getId(), chatRoom);

        return chatRoom;
    }
}
