package com.soundie.comment.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class CommentWithAuthor {

    private Long id;
    private Long memberId;
    private String memberName;
    private Long postId;
    private String content;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    /*
     * fixture 생성 위함
     * */
    public CommentWithAuthor(
            Long id,
            Long memberId,
            String memberName,
            Long postId,
            String content,
            LocalDateTime createdAt
    ){
        this.id = id;
        this.memberId = memberId;
        this.memberName = memberName;
        this.postId = postId;
        this.content = content;
        this.createdAt = createdAt;
    }
}
