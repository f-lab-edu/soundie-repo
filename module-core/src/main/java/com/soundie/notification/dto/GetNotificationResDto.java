package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder
public class GetNotificationResDto {

    private Collection<GetNotificationElement> notifications;

    public static GetNotificationResDto of(List<Notification> notifications) {
        return GetNotificationResDto.builder()
                .notifications(notifications.stream()
                        .map(n -> GetNotificationElement.of(n))
                        .collect(Collectors.toList()))
                .build();

    }
}
