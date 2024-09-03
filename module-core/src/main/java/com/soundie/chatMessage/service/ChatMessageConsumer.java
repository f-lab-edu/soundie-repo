package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.messaging.simp.SimpMessageSendingOperations;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatMessageConsumer {

    private final SimpMessageSendingOperations messagingTemplate;

    /**
     * Kafka 에서 메시지가 발행(publish)되면 대기하고 있던 Kafka Consumer 가 해당 메시지를 받아 처리
     */
    @KafkaListener(topics = "${spring.kafka.template.default-topic}", groupId = "${spring.kafka.consumer.group-id}")
    public void sendMessage(ChatMessage chatMessage) {
        log.info("messageType:{}, messageChatRoomId:{}", chatMessage.getType(),chatMessage.getChatRoomId());
        log.info("messageSenderId:{}, messageContent:{}", chatMessage.getSenderId(),chatMessage.getContent());
        messagingTemplate.convertAndSend(
                "/sub/chatRooms/" + chatMessage.getChatRoomId(),
                chatMessage
        );
    }
}
