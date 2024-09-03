package com.soundie.api;

import com.soundie.chatRoom.dto.ChatRoomIdElement;
import com.soundie.chatRoom.dto.GetChatRoomDetailResDto;
import com.soundie.chatRoom.dto.GetChatRoomResDto;
import com.soundie.chatRoom.dto.PostChatRoomCreateReqDto;
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
    public EnvelopeResponse<GetChatRoomDetailResDto> readChatRoom(@PathVariable Long chatRoomId,
                                                                  @RequestParam Long memberId){
        return EnvelopeResponse.<GetChatRoomDetailResDto>builder()
                .data(chatRoomService.readChatRoom(chatRoomId))
                .build();
    }

    @PostMapping
    public EnvelopeResponse<ChatRoomIdElement> createChatRoom(@RequestBody PostChatRoomCreateReqDto postChatRoomCreateReqDto,
                                                              @RequestParam Long hostMemberId,
                                                              @RequestParam Long guestMemberId){
        return EnvelopeResponse.<ChatRoomIdElement>builder()
                .data(chatRoomService.createChatRoom(hostMemberId, guestMemberId, postChatRoomCreateReqDto))
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
