package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.domain.ChatMessageType;
import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.member.domain.Member;
import lombok.RequiredArgsConstructor;
import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class ChatMessageProducer {

    private final KafkaTemplate<String, ChatMessage> kafkaTemplate;

    private final NewTopic topic;

    public void sendMessage(ChatMessage chatMessage) {
        kafkaTemplate.send(topic.name(), chatMessage);
    }

    public void sendEnterMessage(ChatRoom chatRoom, Member member) {
        String content = member.getName() + "님이 대화를 개설 했습니다.";
        ChatMessage chatMessage = new ChatMessage(
                ChatMessageType.ENTER,
                chatRoom.getId(),
                content,
                LocalDateTime.now()
        );

        kafkaTemplate.send(topic.name(), chatMessage);
    }

    public void sendExitMessage(ChatRoom chatRoom, Member member) {
        String content = member.getName() + "님이 대화를 종료 했습니다.";
        ChatMessage chatMessage = new ChatMessage(
                ChatMessageType.EXIT,
                chatRoom.getId(),
                content,
                LocalDateTime.now()
        );

        kafkaTemplate.send(topic.name(), chatMessage);
    }
}
