package com.soundie.file.dto;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetFileCreateResDto {

    private final String filePath;
    private final String preSignedUrl;

    public static GetFileCreateResDto of(String filePath, String preSignedUrl) {
        return new GetFileCreateResDto(
                filePath,
                preSignedUrl
        );
    }
}
