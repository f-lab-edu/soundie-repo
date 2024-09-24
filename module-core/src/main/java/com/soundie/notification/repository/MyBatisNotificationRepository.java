package com.soundie.notification.repository;

import com.soundie.notification.domain.Notification;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class MyBatisNotificationRepository implements NotificationRepository {

    @Override
    public List<Notification> findNotifications() {
        // 향후 구현
        return List.of();
    }

    @Override
    public List<Notification> findNotificationsByMemberId(Long memberId) {
        // 향후 구현
        return List.of();
    }
}
