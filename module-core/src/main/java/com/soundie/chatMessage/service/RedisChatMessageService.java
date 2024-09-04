package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.repository.RedisChatMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RedisChatMessageService {

    private static final String CHAT_ROOM = "CHAT_ROOM";
    private static final String DELIMITER = ":";

    private final RedisChatMessageRepository redisChatMessageRepository;

    public List<ChatMessage> readMessageList(Long chatRoomId) {
        return redisChatMessageRepository.findChatMessagesByChatRoomId(
                CHAT_ROOM + DELIMITER + chatRoomId
        );
    }

    public ChatMessage createMessage(ChatMessage chatMessage) {
        return redisChatMessageRepository.save(
                CHAT_ROOM + DELIMITER + chatMessage.getChatRoomId(),
                chatMessage);
    }

    public void deleteMessageList(Long chatRoomId) {
        redisChatMessageRepository.delete(
                CHAT_ROOM + DELIMITER + chatRoomId
        );
    }
}
