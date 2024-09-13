package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.repository.ChatMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ChatMessageService {

    private final ChatMessageRepository chatMessageRepository;

    public List<ChatMessage> readMessageList(Long chatRoomId) {
        return chatMessageRepository.findChatMessagesByChatRoomId(chatRoomId);
    }

    public ChatMessage createMessage(ChatMessage chatMessage) {
        return chatMessageRepository.save(chatMessage);
    }

    public void deleteMessageList(Long chatRoomId) {
        chatMessageRepository.deleteChatMessagesByChatRoomId(chatRoomId);
    }
}
