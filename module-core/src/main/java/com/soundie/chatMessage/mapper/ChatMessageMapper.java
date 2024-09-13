package com.soundie.chatMessage.mapper;

import com.soundie.chatMessage.domain.ChatMessage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ChatMessageMapper {

    List<ChatMessage> findChatMessagesByChatRoomId(@Param("chatRoomId") Long chatRoomId);

    Optional<ChatMessage> findChatMessageByChatRoomIdOrderByIdDesc(Long chatRoomId);

    void save(@Param("chatMessage") ChatMessage chatMessage);

    void deleteChatMessagesByChatRoomId(@Param("chatRoomId") Long chatRoomId);
}
