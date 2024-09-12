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
public class Comment {

    private Long id;
    private Long memberId;
    private Long postId;
    private String content;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;

    public Comment(Long memberId, Long postId, String content){
        this.memberId = memberId;
        this.postId = postId;
        this.content = content;
    }

    /*
     * MemoryRepository 저장 위함
     * */
    public void setId(Long id) {
        this.id = id;
    }
}
