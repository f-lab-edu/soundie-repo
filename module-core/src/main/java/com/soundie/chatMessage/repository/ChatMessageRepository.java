package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;

import java.util.List;
import java.util.Optional;

public interface ChatMessageRepository {

    List<ChatMessage> findChatMessagesByChatRoomId(Long chatRoomId);

    List<ChatMessage> findChatMessagesByChatRoomIdOrderByIdDesc(Long chatRoomId, Integer size);

    List<ChatMessage> findChatMessageByChatRoomIdAndIdLessThanOrderByIdDesc(Long chatRoomId, Long cursor, Integer size);

    Optional<ChatMessage> findChatMessageByChatRoomIdOrderByIdDesc(Long chatRoomId);

    ChatMessage save(ChatMessage chatMessage);

    void deleteChatMessagesByChatRoomId(Long chatRoomId);
}
