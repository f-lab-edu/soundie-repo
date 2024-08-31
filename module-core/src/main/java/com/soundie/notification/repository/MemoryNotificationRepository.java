package com.soundie.notification.repository;

import com.soundie.notification.domain.Notification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

@Repository
public class MemoryNotificationRepository implements NotificationRepository {

    private final Map<Long, Notification> store = new ConcurrentHashMap<>();
    private AtomicLong sequence = new AtomicLong(0L);

    /*
     * 알림 목록 조회
     * */
    @Override
    public List<Notification> findNotifications() {
        return new ArrayList<>(store.values());
    }

    /*
     * 사용자 Id로, 알림 목록 조회
     * */
    @Override
    public List<Notification> findNotificationsByMemberId(Long memberId){
        return findNotifications().stream()
                .filter(n -> n.getMemberId().equals(memberId))
                .collect(Collectors.toList());
    }
}
