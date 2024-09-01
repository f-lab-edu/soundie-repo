package com.soundie.notification.dto;

import com.soundie.notification.domain.Notification;
import lombok.Builder;
import lombok.Getter;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Builder(builderMethodName = "innerBuilder")
public class GetNotificationResDto {

    private final Collection<GetNotificationElement> notifications;

    private static GetNotificationResDtoBuilder builder(Collection<GetNotificationElement> notifications) {
        return innerBuilder()
                .notifications(notifications);
    }

    public static GetNotificationResDto of(List<Notification> notifications) {
        return GetNotificationResDto.builder(
                    notifications.stream()
                            .map(GetNotificationElement::of)
                            .collect(Collectors.toList())
                )
                .build();

    }
}
