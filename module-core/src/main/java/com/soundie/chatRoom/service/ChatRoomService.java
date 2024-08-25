package com.soundie.chatRoom.service;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.chatRoom.dto.ChatRoomIdElement;
import com.soundie.chatRoom.dto.GetChatRoomDetailResDto;
import com.soundie.chatRoom.dto.GetChatRoomResDto;
import com.soundie.chatRoom.dto.PostChatRoomCreateReqDto;
import com.soundie.chatRoom.repository.MemoryChatRoomRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final MemoryChatRoomRepository chatRoomRepository;

    public GetChatRoomResDto readChatRoomList(Long memberId) {
        List<ChatRoom> findChatRooms = chatRoomRepository.findChatRoomsByHostMemberIdOrGuestMemberId(memberId);
        return GetChatRoomResDto.of(findChatRooms);
    }

    public GetChatRoomDetailResDto readChatRoom(Long chatRoomId) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        return GetChatRoomDetailResDto.of(findChatRoom);
    }

    public ChatRoomIdElement createChatRoom(Long hostMemberId, Long guestMemberId, PostChatRoomCreateReqDto postChatRoomCreateReqDto) {
        ChatRoom chatRoom = new ChatRoom(
                hostMemberId,
                guestMemberId,
                postChatRoomCreateReqDto.getName(),
                postChatRoomCreateReqDto.getDescription()
        );

        chatRoom = chatRoomRepository.save(chatRoom);

        return ChatRoomIdElement.of(chatRoom.getId());
    }
}
