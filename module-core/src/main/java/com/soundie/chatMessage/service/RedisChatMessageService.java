package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.repository.RedisChatMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class RedisChatMessageService {

    private static final String CHAT_ROOM = "CHAT_ROOM";
    private static final String DELIMITER = ":";

    private final RedisChatMessageRepository redisChatMessageRepository;

    public void createMessage(ChatMessage chatMessage) {
        redisChatMessageRepository.save(
                CHAT_ROOM + DELIMITER + chatMessage.getChatRoomId(),
                chatMessage);
    }
}
