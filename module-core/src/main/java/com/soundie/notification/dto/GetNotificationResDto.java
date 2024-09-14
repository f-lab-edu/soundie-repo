package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetNotificationResDto {

    private final Collection<GetNotificationElement> notifications;

    public static GetNotificationResDto of(List<Notification> notifications) {
        return new GetNotificationResDto(
                notifications.stream()
                        .map(GetNotificationElement::of)
                        .collect(Collectors.toList())
        );
    }
}
