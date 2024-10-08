package com.soundie.chatRoom.service;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatMessage.domain.ChatMessageType;
import com.soundie.chatMessage.dto.GetChatMessageCursorReqDto;
import com.soundie.chatMessage.dto.PostChatMessageCreateReqDto;
import com.soundie.chatMessage.repository.ChatMessageRepository;
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
import com.soundie.global.common.util.PaginationConstant;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
public class ChatRoomService {

    private final ChatRoomRepository chatRoomRepository;
    private final MemberRepository memberRepository;
    private final ChatMessageRepository chatMessageRepository;

    private final ThreadPoolTaskExecutor threadPoolTaskExecutor;

    public GetChatRoomResDto readChatRoomList(Long memberId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        List<ChatRoom> findChatRooms = chatRoomRepository.findChatRoomsByHostMemberIdOrGuestMemberId(findMember.getId());
        return GetChatRoomResDto.of(findChatRooms);
    }

    public GetChatRoomDetailResDto readChatRoom(Long chatRoomId, Long memberId, GetChatMessageCursorReqDto getChatMessageCursorReqDto) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        // Host 회원과 Guest 회원이 아닌, 회원이 채팅방 접속
        if (!findMember.getId().equals(findChatRoom.getHostMemberId()) && !findMember.getId().equals(findChatRoom.getGuestMemberId())){
            throw new BadRequestException(ApplicationError.INVALID_AUTHORITY);
        }

        Long cursor = getChatMessageCursorReqDto.getCursor();
        Integer size = getChatMessageCursorReqDto.getSize();

        List<ChatMessage> findChatMessages = findChatMessagesCursorCheckExistsCursor(findChatRoom.getId(), cursor, size);
        return GetChatRoomDetailResDto.of(findChatRoom, findChatMessages, size);
    }

    public ChatRoomIdElement createChatRoom(Long memberId, PostChatRoomCreateReqDto postChatRoomCreateReqDto) {
        Member findHostMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Member findGuestMember = memberRepository.findMemberById(postChatRoomCreateReqDto.getGuestMemberId())
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        // Host 회원과 Guest 회원, 채팅방이 이미 존재
        boolean hasChatRoom = chatRoomRepository.findChatRoomByHostMemberIdAndGuestMemberId(findHostMember.getId(), findGuestMember.getId())
                .isPresent();
        if (hasChatRoom) {
            throw new DuplicateException(ApplicationError.DUPLICATE_CHAT_ROOM);
        }

        // Host 회원과 Guest 회원이 반대인, 채팅방이 이미 존재
        boolean hasReverseChatRoom = chatRoomRepository.findChatRoomByHostMemberIdAndGuestMemberId(findGuestMember.getId(), findHostMember.getId())
                .isPresent();
        if (hasReverseChatRoom) {
            throw new DuplicateException(ApplicationError.DUPLICATE_CHAT_ROOM);
        }

        CompletableFuture<ChatRoom> chatRoomFuture = CompletableFuture.supplyAsync(() -> {
            // 채팅방 생성
            ChatRoom chatRoom = new ChatRoom(
                    findHostMember.getId(),
                    findGuestMember.getId(),
                    postChatRoomCreateReqDto.getName(),
                    postChatRoomCreateReqDto.getDescription()
            );
            chatRoom = chatRoomRepository.save(chatRoom);

            return chatRoom;
        }, threadPoolTaskExecutor).thenApplyAsync(chatRoom -> {
            // 입장 메시지 생성
            ChatMessage enterChatMessage = new ChatMessage(
                    chatRoom.getId(),
                    ChatUtil.ADMIN_ID,
                    ChatMessageType.ENTER,
                    findHostMember.getName() + ChatUtil.ENTER_MESSAGE,
                    ChatUtil.INITIAL_MEMBER_CNT
            );
            chatMessageRepository.save(enterChatMessage);

            return chatRoom;
        }, threadPoolTaskExecutor);

        return ChatRoomIdElement.of(chatRoomFuture.join());
    }

    public ChatRoomIdElement sendChatRoomByMessage(Long chatRoomId, Long memberId, PostChatMessageCreateReqDto postChatMessageCreateReqDto) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        // Host 회원과 Guest 회원이 아닌, 회원이 채팅방 접속
        if (!findMember.getId().equals(findChatRoom.getHostMemberId()) && !findMember.getId().equals(findChatRoom.getGuestMemberId())){
            throw new BadRequestException(ApplicationError.INVALID_AUTHORITY);
        }

        // 전송 메시지 생성
        ChatMessage talkChatMessage = new ChatMessage(
                findChatRoom.getId(),
                findMember.getId(),
                ChatMessageType.TALK,
                postChatMessageCreateReqDto.getContent(),
                postChatMessageCreateReqDto.getContentImgPath(),
                postChatMessageCreateReqDto.getMemberCnt()
        );
        chatMessageRepository.save(talkChatMessage);

        return ChatRoomIdElement.of(findChatRoom);
    }

    public ChatRoomIdElement deleteChatRoom(Long chatRoomId, Long memberId) {
        ChatRoom findChatRoom = chatRoomRepository.findChatRoomById(chatRoomId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_ROOM_NOT_FOUND));
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        // Host 회원과 Guest 회원이 아닌, 회원이 채팅방 나감
        if (!findMember.getId().equals(findChatRoom.getHostMemberId()) && !findMember.getId().equals(findChatRoom.getGuestMemberId())){
            throw new BadRequestException(ApplicationError.INVALID_AUTHORITY);
        }

        ChatMessage findRecentChatMessage = chatMessageRepository.findChatMessageByChatRoomIdOrderByIdDesc(findChatRoom.getId())
                .orElseThrow(() -> new NotFoundException(ApplicationError.CHAT_MESSAGE_NOT_FOUND));

        return toggleExitChatRoom(findChatRoom, findMember, findRecentChatMessage);
    }

    private ChatRoomIdElement toggleExitChatRoom(ChatRoom chatRoom, Member member, ChatMessage chatMessage) {
        // 채팅방 모두 나감 X
        if (chatMessage.getMemberCnt() == ChatUtil.INITIAL_MEMBER_CNT){
            CompletableFuture.runAsync(() -> {
                // 채팅방 나간 인원, null 처리
                chatRoomRepository.updateMemberNullIfMatchMember(chatRoom, member);
            }, threadPoolTaskExecutor).thenRunAsync(() -> {
                // 퇴장 메시지 생성
                ChatMessage exitChatMessage = new ChatMessage(
                        chatRoom.getId(),
                        ChatUtil.ADMIN_ID,
                        ChatMessageType.EXIT,
                        member.getName() + ChatUtil.EXIT_MESSAGE,
                        ChatUtil.INITIAL_MEMBER_CNT - 1
                );
                chatMessageRepository.save(exitChatMessage);
            }, threadPoolTaskExecutor);

            return ChatRoomIdElement.of(chatRoom);
        }

        // 채팅방 모두 나감
        chatRoomRepository.delete(chatRoom);
        return ChatRoomIdElement.of(chatRoom);
    }

    private List<ChatMessage> findChatMessagesCursorCheckExistsCursor(Long chatRoomId, Long cursor, Integer size) {
        return cursor.equals(PaginationConstant.START_CURSOR) ? chatMessageRepository.findChatMessagesByChatRoomIdOrderByIdDesc(chatRoomId, size)
                : chatMessageRepository.findChatMessageByChatRoomIdAndIdLessThanOrderByIdDesc(chatRoomId, cursor, size);
    }
}
