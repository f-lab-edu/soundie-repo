package com.soundie.api;

import com.soundie.global.common.dto.EnvelopeResponse;
import com.soundie.image.dto.GetImageCreateReqDto;
import com.soundie.image.dto.GetImageCreateResDto;
import com.soundie.image.service.ImageService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/images")
public class ImageController {

    private final ImageService imageService;

    @GetMapping("/url")
    public EnvelopeResponse<GetImageCreateResDto> getPreSignedUrl(@RequestBody GetImageCreateReqDto getImageCreateReqDto,
                                                                  @RequestParam Long memberId){
        return EnvelopeResponse.<GetImageCreateResDto>builder()
                .data(imageService.getPreSignedUrl(memberId, getImageCreateReqDto))
                .build();
    }
}
