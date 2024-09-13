package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;

import java.util.List;
import java.util.Optional;

public interface ChatMessageRepository {

    List<ChatMessage> findChatMessagesByChatRoomId(Long chatRoomId);

    Optional<ChatMessage> findChatMessageByChatRoomIdOrderByIdDesc(Long chatRoomId);

    ChatMessage save(ChatMessage chatMessage);

    void deleteChatMessagesByChatRoomId(Long chatRoomId);
}
