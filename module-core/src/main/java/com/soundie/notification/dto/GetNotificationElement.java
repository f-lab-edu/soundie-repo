package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import com.soundie.notification.domain.NotificationType;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetNotificationElement {

    private final Long notificationId;
    private final String message;
    private final Long contentId;
    private final NotificationType notificationType;
    private final LocalDateTime createdAt;
    private final Boolean read;

    public static GetNotificationElement of(Notification notification) {
        return new GetNotificationElement(
                notification.getId(),
                notification.getMessage(),
                notification.getContentId(),
                notification.getNotificationType(),
                notification.getCreatedAt(),
                notification.getRead()
        );
    }
}
