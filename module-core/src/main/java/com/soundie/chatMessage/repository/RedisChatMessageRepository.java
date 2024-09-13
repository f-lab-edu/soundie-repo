package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@RequiredArgsConstructor
public class RedisChatMessageRepository implements ChatMessageRepository {

    private static final String CHAT_ROOM = "CHAT_ROOM";
    private static final String DELIMITER = "::";

    private final RedisTemplate<String, Object> redisTemplate;  // redis

    /*
     * 채팅방 Id를 key 로, 채팅 메시지 목록 조회
     * */
    @Override
    public List<ChatMessage> findChatMessagesByChatRoomId(Long chatRoomId) {
        HashOperations<String, String, ChatMessage> hashOperations = redisTemplate.opsForHash();
        return hashOperations.values(CHAT_ROOM + DELIMITER + chatRoomId);
    }

    /*
     * 채팅방 Id를 key 로, 채팅 메시지 저장
     * */
    @Override
    public ChatMessage save(ChatMessage chatMessage) {
        HashOperations<String, String, ChatMessage> hashOperations = redisTemplate.opsForHash();
        hashOperations.put(
                CHAT_ROOM + DELIMITER + chatMessage.getChatRoomId(),
                UUID.randomUUID().toString(),
                chatMessage
        );
        return chatMessage;
    }

    /*
     * 채팅방 Id를 key 로, 채팅 메시지 목록 삭제
     * */
    @Override
    public void deleteChatMessagesByChatRoomId(Long chatRoomId) {
        HashOperations<String, String, ChatMessage> hashOperations = redisTemplate.opsForHash();
        Map<String, ChatMessage> entries = hashOperations.entries(
                CHAT_ROOM + DELIMITER + chatRoomId
        );
        entries.forEach((subKey, redisChatMessage)
                -> hashOperations.delete(CHAT_ROOM + DELIMITER + chatRoomId, subKey)
        );
    }
}
