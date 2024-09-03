package com.soundie.chatMessage.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.domain.ChatMessageType;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.apache.kafka.clients.admin.NewTopic;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ChatMessageProducer {

    private final KafkaTemplate<String, ChatMessage> kafkaTemplate;

    private final NewTopic topic;
    private final MemberRepository memberRepository;

    public void sendMessage(ChatMessage chatMessage) {
        if (ChatMessageType.ENTER.equals(chatMessage.getType())) {

            Member findMember = memberRepository.findMemberById(chatMessage.getSenderId())
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

            String enterContent = findMember.getName() + "님이 입장하셨습니다.";
            chatMessage.setContent(enterContent);
        }

        kafkaTemplate.send(topic.name(), chatMessage);
    }
}
