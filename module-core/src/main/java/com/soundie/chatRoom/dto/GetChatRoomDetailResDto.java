package com.soundie.chatRoom.dto;

import com.soundie.chatMessage.domain.ChatMessage;
import com.soundie.chatRoom.domain.ChatRoom;
import lombok.*;

import java.util.List;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetChatRoomDetailResDto {

    private GetChatRoomDetailElement chatRoom;
    private List<ChatMessage> chatMessages;
    private Long cursor;

    public static GetChatRoomDetailResDto of(ChatRoom chatRoom, List<ChatMessage> chatMessages, Integer size) {
        return new GetChatRoomDetailResDto(
                    GetChatRoomDetailElement.of(chatRoom),
                    chatMessages,
                    getNextCursor(chatMessages, size)
        );
    }

    private static Long getNextCursor(List<ChatMessage> chatMessages, Integer size) {
        Long nextCursor = null;
        if (chatMessages.size() == size) {
            nextCursor = chatMessages.get(size - 1).getId();
        }

        return nextCursor;
    }
}
