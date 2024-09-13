package com.soundie.webSocket;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.service.ChatMessageProducer;
import com.soundie.chatMessage.service.ChatMessageService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class ChatMessageController {

    private final ChatMessageProducer chatMessageProducer;
    private final ChatMessageService chatMessageService;

    @MessageMapping("/chatMessage")
    public void message(ChatMessage chatMessage) {
        chatMessageProducer.sendMessage(chatMessage);
        ChatMessage saveMessage = chatMessageService.createMessage(chatMessage);
    }
}
