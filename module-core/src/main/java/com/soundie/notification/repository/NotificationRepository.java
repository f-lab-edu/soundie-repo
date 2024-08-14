package com.soundie.notification.repository;

import com.soundie.notification.domain.Notification;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Repository
public class NotificationRepository {

    private static final Map<Long, Notification> store = new HashMap<>(); //static
    private static long sequence = 0L; //static

    /*
     * 알림 목록 조회
     * */
    public List<Notification> findNotifications() {
        return new ArrayList<>(store.values());
    }

    /*
     * 사용자 Id로, 알림 목록 조회
     * */
    public List<Notification> findNotificationsByMemberId(Long memberId){
        return findNotifications().stream()
                .filter(n -> n.getMemberId().equals(memberId))
                .collect(Collectors.toList());
    }
}
