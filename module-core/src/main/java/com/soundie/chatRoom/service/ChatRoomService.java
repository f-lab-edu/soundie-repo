package com.soundie.chatRoom.service;

import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.chatRoom.dto.ChatRoomIdElement;
import com.soundie.chatRoom.dto.GetChatRoomDetailResDto;
import com.soundie.chatRoom.dto.GetChatRoomResDto;
import com.soundie.chatRoom.dto.PostChatRoomCreateReqDto;
import com.soundie.chatRoom.repository.ChatRoomRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.BadRequestException;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final ChatRoomRepository chatRoomRepository;
    private final MemberRepository memberRepository;

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

        return ChatRoomIdElement.of(chatRoom);
    }

    public ChatRoomIdElement deleteChatRoom(Long chatRoomId, Long memberId) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        if (!findChatRoom.getHostMemberId().equals(findMember.getId())){
            throw new BadRequestException(ApplicationError.INVALID_AUTHORITY);
        }

        chatRoomRepository.delete(findChatRoom);

        return ChatRoomIdElement.ofId(chatRoomId);
    }
}
