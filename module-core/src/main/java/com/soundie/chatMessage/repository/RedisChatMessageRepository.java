package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class RedisChatMessageRepository {
    private final RedisTemplate<String, Object> redisTemplate;  // redis

    /*
    * 채팅방 Id를 key 로, 채팅 메시지 목록 조회
    * */
    public List<ChatMessage> findChatMessagesByChatRoomId(String chatRoomKey) {
        HashOperations<String, String, ChatMessage> hashOperations = redisTemplate.opsForHash();
        return hashOperations.values(chatRoomKey);
    }

    /*
    * 채팅방 Id를 key 로, 채팅 메시지 저장
    * */
    public ChatMessage save(String chatRoomKey, ChatMessage chatMessage) {
        HashOperations<String, String, ChatMessage> hashOperations = redisTemplate.opsForHash();
        hashOperations.put(chatRoomKey, chatMessage.getId(), chatMessage); // converter 필요
        return chatMessage;
    }
}
