package com.soundie.chatMessage.mapper;

import com.soundie.chatMessage.domain.ChatMessage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ChatMessageMapper {

    List<ChatMessage> findChatMessagesByChatRoomId(@Param("chatRoomId") Long chatRoomId);

    List<ChatMessage> findChatMessagesByChatRoomIdOrderByIdDesc(
            @Param("chatRoomId") Long chatRoomId,
            @Param("size") Integer size);

    List<ChatMessage> findChatMessageByChatRoomIdAndIdLessThanOrderByIdDesc(
            @Param("chatRoomId") Long chatRoomId,
            @Param("id") Long chatMessageId,
            @Param("size") Integer size);

    Optional<ChatMessage> findChatMessageByChatRoomIdOrderByIdDesc(Long chatRoomId);

    void save(@Param("chatMessage") ChatMessage chatMessage);

    void deleteChatMessagesByChatRoomId(@Param("chatRoomId") Long chatRoomId);
}
