package com.soundie.notification.domain;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class Notification {

    private Long id;
    private Long memberId;
    private Long ownerId;
    private String message;
    private Long contentId;
    private NotificationType notificationType;
    private Boolean read;
    private LocalDateTime createdAt;

    public Notification(Long memberId, Long ownerId, String message, Long contentId, NotificationType notificationType){
        this.memberId = memberId;
        this.ownerId = ownerId;
        this.message = message;
        this.contentId = contentId;
        this.notificationType = notificationType;
    }
}
