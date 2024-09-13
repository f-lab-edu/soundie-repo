package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.repository.RedisChatMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RedisChatMessageService {

    private final RedisChatMessageRepository redisChatMessageRepository;

    public List<ChatMessage> readMessageList(Long chatRoomId) {
        return redisChatMessageRepository.findChatMessagesByChatRoomId(chatRoomId);
    }

    public ChatMessage createMessage(ChatMessage chatMessage) {
        return redisChatMessageRepository.save(chatMessage);
    }

    public void deleteMessageList(Long chatRoomId) {
        redisChatMessageRepository.deleteChatMessagesByChatRoomId(chatRoomId);
    }
}
