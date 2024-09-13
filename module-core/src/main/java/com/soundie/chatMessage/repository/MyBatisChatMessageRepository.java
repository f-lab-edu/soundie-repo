package com.soundie.chatMessage.repository;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.mapper.ChatMessageMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class MyBatisChatMessageRepository implements ChatMessageRepository {

    private final ChatMessageMapper chatMessageMapper;

    /*
     * 채팅방 Id로, 채팅 메시지 목록 조회
     * */
    @Override
    public List<ChatMessage> findChatMessagesByChatRoomId(Long chatRoomId) {
        return chatMessageMapper.findChatMessagesByChatRoomId(chatRoomId);
    }

    /*
     * 채팅방 Id로, 최근 채팅 메시지 조회
     * */
    @Override
    public ChatMessage findChatMessageByChatRoomIdOrderByIdDesc(Long chatRoomId) {
        return chatMessageMapper.findChatMessageByChatRoomIdOrderByIdDesc(chatRoomId);
    }

    /*
     * 채팅방 Id로, 채팅 메시지 저장
     * */
    @Override
    public ChatMessage save(ChatMessage chatMessage) {
        chatMessageMapper.save(chatMessage);
        return chatMessage;
    }

    /*
     * 채팅방 Id로, 채팅 메시지 목록 삭제
     * */
    @Override
    public void deleteChatMessagesByChatRoomId(Long chatRoomId) {
        chatMessageMapper.deleteChatMessagesByChatRoomId(chatRoomId);
    }
}
