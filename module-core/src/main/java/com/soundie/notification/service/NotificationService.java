package com.soundie.notification.service;

import com.soundie.notification.domain.Notification;
import com.soundie.notification.dto.GetNotificationResDto;
import com.soundie.notification.repository.NotificationRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class NotificationService {

    private final NotificationRepository notificationRepository;

    public GetNotificationResDto readNotificationList(Long memberId) {
        List<Notification> findNotifications = notificationRepository.findNotificationsByMemberId(memberId);
        return GetNotificationResDto.of(findNotifications);
    }
}
