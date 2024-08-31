package com.soundie.api;

import com.soundie.global.common.dto.EnvelopeResponse;
import com.soundie.notification.dto.GetNotificationResDto;
import com.soundie.notification.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/notifications")
public class NotificationController {

    private final NotificationService notificationService;

    @GetMapping
    public EnvelopeResponse<GetNotificationResDto> readNotifications(@RequestParam Long memberId){
        return EnvelopeResponse.<GetNotificationResDto>builder()
                .data(notificationService.readNotificationList(memberId))
                .build();
    }
}
