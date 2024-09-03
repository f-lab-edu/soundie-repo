package com.soundie.webSocket;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.service.ChatMessageProducer;
import com.soundie.chatMessage.service.RedisChatMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;

import java.util.UUID;

@Controller
@RequiredArgsConstructor
public class ChatMessageController {

    private final ChatMessageProducer chatMessageProducer;
    private final RedisChatMessageService redisChatMessageService;

    @MessageMapping("/chatMessage")
    public void message(ChatMessage chatMessage) {
        chatMessage.setId(UUID.randomUUID().toString());
        chatMessageProducer.sendMessage(chatMessage);
        redisChatMessageService.createMessage(chatMessage);
    }
}
