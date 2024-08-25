package com.soundie.post.controller;

import com.soundie.ControllerTestSupport;
import com.soundie.post.dto.*;
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

class PostControllerTest extends ControllerTestSupport {

    @DisplayName("음원 게시물 목록을 조회한다.")
    @Test
    void readPostList() throws Exception {
        // when // then
        mockMvc.perform(
                    get("/api/posts")
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.posts").isArray());
    }

    @DisplayName("음원 게시물을 조회한다.")
    @Test
    void readPost() throws Exception {
        //given
        Long postId = 1L; //pathVariable

        // when // then
        mockMvc.perform(
                    get("/api/posts/" + postId)
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.post").exists());
    }

    @DisplayName("음원 게시물을 등록한다.")
    @Test
    void createPost() throws Exception {
        // given
        Long memberId = 1L;
        MultiValueMap <String, String> params = new LinkedMultiValueMap<>();
        params.add("memberId", String.valueOf(memberId));

        PostPostCreateReqDto request = postPostCreateReqDto();

        // when // then
        mockMvc.perform(
                    post("/api/posts")
                            .params(params)
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request))
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.postId").exists());
    }

    @DisplayName("음원 게시물에 좋아요를 누른다.")
    @Test
    void likePost() throws Exception {
        // given
        Long postId = 1L; //pathVariable

        Long memberId = 1L;
        MultiValueMap <String, String> params = new LinkedMultiValueMap<>();
        params.add("memberId", String.valueOf(memberId));

        // when // then
        mockMvc.perform(
                    post("/api/posts/" + postId + "/like")
                            .params(params)
                            .contentType(MediaType.APPLICATION_JSON)
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.likeCount").exists())
                .andExpect(jsonPath("$.data.liked").exists());
    }

    private PostPostCreateReqDto postPostCreateReqDto(){
        return new PostPostCreateReqDto(
                "노래 제목",
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );
    }
}
