package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import com.soundie.notification.domain.NotificationType;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class GetNotificationElement {

    private Long notificationId;
    private String message;
    private Long contentId;
    private NotificationType notificationType;
    private LocalDateTime createdAt;
    private Boolean read;


    public static GetNotificationElement of(Notification notification) {
        return GetNotificationElement.builder()
                .notificationId(notification.getId())
                .message(notification.getMessage())
                .contentId(notification.getContentId())
                .notificationType(notification.getNotificationType())
                .createdAt(notification.getCreatedAt())
                .read(notification.getRead())
                .build();
    }
}
