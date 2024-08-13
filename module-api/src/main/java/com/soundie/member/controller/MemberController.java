package com.soundie.member.controller;

import com.soundie.global.common.EnvelopeResponse;
import com.soundie.member.dto.MemberIdElement;
import com.soundie.member.service.MemberService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/members")
public class MemberController {

    private final MemberService memberService;

    @PostMapping
    public EnvelopeResponse createMember(){
        MemberIdElement memberId = memberService.createMember();
        return new EnvelopeResponse<>("200", "success", memberId);
    }
}
