package com.soundie.image.service;

import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.image.dto.GetImageCreateReqDto;
import com.soundie.image.dto.GetImageCreateResDto;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.DeleteObjectRequest;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest;

import java.time.Duration;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ImageService {

    private final S3Presigner s3Presigner;
    private final S3Client s3Client;
    private final MemberRepository memberRepository;

    @Value("${cloud.aws.s3.bucket}")
    private String bucket;

    @Value("${cloud.aws.s3.expire-in}")
    private Long expireIn;

    public GetImageCreateResDto getPreSignedUrl(Long memberId, GetImageCreateReqDto getImageCreateReqDto) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        String imageTypeName = getImageCreateReqDto.getTypeName();
        String fileName = makeFileName(imageTypeName);

        PutObjectRequest putObjectRequest = makePutObjectRequest(bucket, fileName);
        PutObjectPresignRequest putObjectPresignRequest = makePutObjectPreSignRequest(putObjectRequest);
        return GetImageCreateResDto.of(
                fileName,
                s3Presigner.presignPutObject(putObjectPresignRequest).url().toExternalForm()
        );
    }

    public void deleteFile(String filePath) {
        DeleteObjectRequest deleteObjectRequest = DeleteObjectRequest.builder()
                .bucket(bucket)
                .key(filePath)
                .build();

        s3Client.deleteObject(deleteObjectRequest);
    }

    private PutObjectPresignRequest makePutObjectPreSignRequest(PutObjectRequest PutObjectRequest) {
        return PutObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(expireIn))
                .putObjectRequest(PutObjectRequest)
                .build();
    }

    private PutObjectRequest makePutObjectRequest(String bucket, String fileName) {
        return PutObjectRequest.builder()
                .bucket(bucket)
                .key(fileName)
                .build();
    }

    private String makeFileName(String imageTypeName) {
        return new StringBuffer().append(imageTypeName)
                .append("/")
                .append(UUID.randomUUID())
                .toString();
    }
}
