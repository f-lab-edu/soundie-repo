package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import com.soundie.notification.domain.NotificationType;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetNotificationElement {

    private final Long notificationId;
    private final String message;
    private final Long contentId;
    private final NotificationType notificationType;
    private final LocalDateTime createdAt;
    private final Boolean read;

    private static GetNotificationElementBuilder builder(
            Long notificationId,
            String message,
            Long contentId,
            NotificationType notificationType,
            LocalDateTime createdAt,
            Boolean read) {
        return innerBuilder()
                .notificationId(notificationId)
                .message(message)
                .contentId(contentId)
                .notificationType(notificationType)
                .createdAt(createdAt)
                .read(read);
    }

    public static GetNotificationElement of(Notification notification) {
        return GetNotificationElement.builder(
                    notification.getId(),
                    notification.getMessage(),
                    notification.getContentId(),
                    notification.getNotificationType(),
                    notification.getCreatedAt(),
                    notification.getRead()
                )
                .build();
    }
}
