package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;

import java.util.List;

public interface ChatMessageRepository {

    List<ChatMessage> findChatMessagesByChatRoomId(Long chatRoomId);

    ChatMessage save(ChatMessage chatMessage);

    void deleteChatMessagesByChatRoomId(Long chatRoomId);
}
