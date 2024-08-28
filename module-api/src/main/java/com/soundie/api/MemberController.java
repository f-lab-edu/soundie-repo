package com.soundie.api;

import com.soundie.global.common.dto.EnvelopeResponse;
import com.soundie.member.dto.MemberIdElement;
import com.soundie.member.dto.PostMemberCreateReqDto;
import com.soundie.member.service.MemberService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/members")
public class MemberController {

    private final MemberService memberService;

    @PostMapping
    public EnvelopeResponse<MemberIdElement> createMember(@RequestBody PostMemberCreateReqDto postMemberCreateReqDto){
        return EnvelopeResponse.<MemberIdElement>builder()
                .data(memberService.createMember(postMemberCreateReqDto))
                .build();
    }
}
