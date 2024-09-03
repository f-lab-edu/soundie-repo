package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import lombok.RequiredArgsConstructor;
import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ChatMessageProducer {

    private final KafkaTemplate<String, ChatMessage> kafkaTemplate;

    private final NewTopic topic;

    public void sendMessage(ChatMessage chatMessage) {
        kafkaTemplate.send(topic.name(), chatMessage);
    }
}
