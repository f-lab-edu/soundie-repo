package com.soundie.chatRoom.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.domain.ChatMessageType;
import com.soundie.chatMessage.service.ChatMessageProducer;
import com.soundie.chatMessage.service.ChatMessageService;
import com.soundie.chatRoom.domain.ChatRoom;
import com.soundie.chatRoom.dto.ChatRoomIdElement;
import com.soundie.chatRoom.dto.GetChatRoomDetailResDto;
import com.soundie.chatRoom.dto.GetChatRoomResDto;
import com.soundie.chatRoom.dto.PostChatRoomCreateReqDto;
import com.soundie.chatRoom.repository.ChatRoomRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.BadRequestException;
import com.soundie.global.common.exception.DuplicateException;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.global.common.util.ChatUtil;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final ChatRoomRepository chatRoomRepository;
    private final MemberRepository memberRepository;

    private final ChatMessageProducer chatMessageProducer;
    private final ChatMessageService chatMessageService;

    public GetChatRoomResDto readChatRoomList(Long memberId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        List<ChatRoom> findChatRooms = chatRoomRepository.findChatRoomsByHostMemberIdOrGuestMemberId(findMember.getId());
        return GetChatRoomResDto.of(findChatRooms);
    }

    public GetChatRoomDetailResDto readChatRoom(Long chatRoomId, Long memberId) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        List<ChatMessage> findChatMessages = chatMessageService.readMessageList(findChatRoom.getId());

        return GetChatRoomDetailResDto.of(findChatRoom, findChatMessages);
    }

    public ChatRoomIdElement createChatRoom(Long hostMemberId, Long guestMemberId, PostChatRoomCreateReqDto postChatRoomCreateReqDto) {
        Member findHostMember = memberRepository.findMemberById(hostMemberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Member findGuestMember = memberRepository.findMemberById(guestMemberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        // Host 회원과 Guest 회원, 채팅방이 이미 존재
        Boolean hasChatRoom = chatRoomRepository.findChatRoomByHostMemberIdAndGuestMemberId(findHostMember.getId(), findGuestMember.getId())
                .isPresent();
        if (hasChatRoom) {
            throw new DuplicateException(ApplicationError.DUPLICATE_CHAT_ROOM);
        }

        // Host 회원과 Guest 회원이 반대인, 채팅방이 이미 존재
        Boolean hasReverseChatRoom = chatRoomRepository.findChatRoomByHostMemberIdAndGuestMemberId(findGuestMember.getId(), findHostMember.getId())
                .isPresent();
        if (hasReverseChatRoom) {
            throw new DuplicateException(ApplicationError.DUPLICATE_CHAT_ROOM);
        }

        ChatRoom chatRoom = new ChatRoom(
                findHostMember.getId(),
                findGuestMember.getId(),
                postChatRoomCreateReqDto.getName(),
                postChatRoomCreateReqDto.getDescription()
        );

        chatRoom = chatRoomRepository.save(chatRoom);

        // 메시지 생성
        ChatMessage chatMessage = new ChatMessage(
                chatRoom.getId(),
                ChatUtil.ADMIN_ID,
                ChatMessageType.ENTER,
                findGuestMember.getName() + ChatUtil.ENTER_MESSAGE,
                LocalDateTime.now()
        );
        chatMessageProducer.sendMessage(chatMessage);
        chatMessageService.createMessage(chatMessage);

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
        chatMessageService.deleteMessageList(findChatRoom.getId());

        return ChatRoomIdElement.ofId(chatRoomId);
    }
}
