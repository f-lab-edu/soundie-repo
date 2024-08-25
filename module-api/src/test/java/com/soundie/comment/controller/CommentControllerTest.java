package com.soundie.comment.controller;

import com.soundie.ControllerTestSupport;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class CommentControllerTest extends ControllerTestSupport {

    @DisplayName("음원 게시물에 달린 댓글 목록을 조회한다.")
    @Test
    void readComments() throws Exception {
        //given
        Long postId = 1L;
        MultiValueMap <String, String> params = new LinkedMultiValueMap<>();
        params.add("postId", String.valueOf(postId));

        // when // then
        mockMvc.perform(
                    get("/api/comments")
                            .params(params)
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.comments").isArray());
    }

    @DisplayName("음원 게시물에 댓글을 등록한다.")
    @Test
    void createComment() throws Exception {
        // given
        Long memberId = 1L;
        Long postId = 1L;
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("memberId", String.valueOf(memberId));
        params.add("postId", String.valueOf(postId));

        PostCommentCreateReqDto request = postCommentCreateReqDto();

        // when // then
        mockMvc.perform(
                    post("/api/comments")
                            .params(params)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request))
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.commentId").exists());
    }

    private PostCommentCreateReqDto postCommentCreateReqDto(){
        return new PostCommentCreateReqDto(
                "댓글 내용"
        );
    }
}