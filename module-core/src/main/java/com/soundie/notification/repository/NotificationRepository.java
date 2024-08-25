package com.soundie.notification.repository;

import com.soundie.notification.domain.Notification;

import java.util.List;

public interface NotificationRepository {

    List<Notification> findNotifications();

    List<Notification> findNotificationsByMemberId(Long memberId);
}
