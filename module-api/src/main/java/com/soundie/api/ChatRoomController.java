package com.soundie.api;

import com.soundie.chatMessage.dto.GetChatMessageCursorReqDto;
import com.soundie.chatMessage.dto.PostChatMessageCreateReqDto;
import com.soundie.chatRoom.dto.*;
import com.soundie.chatRoom.service.ChatRoomService;
import com.soundie.global.common.dto.EnvelopeResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/chatRooms")
public class ChatRoomController {

    private final ChatRoomService chatRoomService;

    @GetMapping
    public EnvelopeResponse<GetChatRoomResDto> readChatRooms(@RequestParam Long memberId){
        return EnvelopeResponse.<GetChatRoomResDto>builder()
                .data(chatRoomService.readChatRoomList(memberId))
                .build();
    }

    @GetMapping("/{chatRoomId}")
    public EnvelopeResponse<GetChatRoomDetailResDto> readChatRoom(@RequestBody GetChatMessageCursorReqDto getChatMessageCursorReqDto,
                                                                  @PathVariable Long chatRoomId,
                                                                  @RequestParam Long memberId){
        return EnvelopeResponse.<GetChatRoomDetailResDto>builder()
                .data(chatRoomService.readChatRoom(chatRoomId, memberId, getChatMessageCursorReqDto))
                .build();
    }

    @PostMapping
    public EnvelopeResponse<ChatRoomIdElement> createChatRoom(@RequestBody PostChatRoomCreateReqDto postChatRoomCreateReqDto,
                                                              @RequestParam Long memberId){
        return EnvelopeResponse.<ChatRoomIdElement>builder()
                .data(chatRoomService.createChatRoom(memberId, postChatRoomCreateReqDto))
                .build();
    }

    @PostMapping("/{chatRoomId}")
    public EnvelopeResponse<ChatRoomIdElement> sendChatRoomByMessage(@RequestBody PostChatMessageCreateReqDto postChatMessageCreateReqDto,
                                                                     @PathVariable Long chatRoomId,
                                                                     @RequestParam Long memberId) {
        return EnvelopeResponse.<ChatRoomIdElement>builder()
                .data(chatRoomService.sendChatRoomByMessage(chatRoomId, memberId, postChatMessageCreateReqDto))
                .build();
    }

    @DeleteMapping("/{chatRoomId}")
    public EnvelopeResponse<ChatRoomIdElement> deleteChatRoom(@PathVariable Long chatRoomId,
                                                              @RequestParam Long memberId) {
        return EnvelopeResponse.<ChatRoomIdElement>builder()
                .data(chatRoomService.deleteChatRoom(chatRoomId, memberId))
                .build();
    }
}
