package com.soundie.image.dto;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GetImageCreateResDto {

    private final String imagePath;
    private final String preSignedUrl;

    public static GetImageCreateResDto of(String imagePath, String preSignedUrl) {
        return new GetImageCreateResDto(
                imagePath,
                preSignedUrl
        );
    }
}
