package com.soundie.api;

import com.soundie.global.common.dto.EnvelopeResponse;
import com.soundie.file.dto.GetFileCreateReqDto;
import com.soundie.file.dto.GetFileCreateResDto;
import com.soundie.file.service.FileService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/files")
public class FileController {

    private final FileService fileService;

    @GetMapping("/url")
    public EnvelopeResponse<GetFileCreateResDto> getPreSignedUrl(@RequestBody GetFileCreateReqDto getFileCreateReqDto,
                                                                 @RequestParam Long memberId){
        return EnvelopeResponse.<GetFileCreateResDto>builder()
                .data(fileService.getPreSignedUrl(memberId, getFileCreateReqDto))
                .build();
    }
}
