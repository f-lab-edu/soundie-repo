package com.soundie.post.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import com.soundie.post.dto.*;
import com.soundie.post.service.PostService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = PostController.class)
class PostControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private PostService postService;

    @DisplayName("음원 게시물 목록을 조회한다.")
    @Test
    void readPostList() throws Exception {
        // given
        GetPostResDto result = GetPostResDto.of(List.of());

        given(postService.readPostList()).willReturn(result);

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
        Long memberId = 1L;
        Member member = new Member("");
        Long postId = 1L;
        Post post = new Post(
                memberId,
                "", "", "", "", ""
        );

        GetPostDetailResDto result = GetPostDetailResDto.of(post, member);

        given(postService.readPost(memberId, postId)).willReturn(result);

        // when // then
        mockMvc.perform(
                        get("/api/posts/" + postId)
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"));
    }

    @DisplayName("음원 게시물을 등록한다.")
    @Test
    void createPost() throws Exception {
        // given
        Long memberId = 1L;
        Long postId = 1L;

        PostPostCreateReqDto request = postPostCreateReqDto();
        PostIdElement response = postIdElement(postId);
        given(postService.createPost(eq(memberId), any(PostPostCreateReqDto.class))).willReturn(response);

        // when // then
        MultiValueMap <String, String> params = new LinkedMultiValueMap<>();
        params.add("memberId", String.valueOf(memberId));
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
                .andExpect(jsonPath("$.data.postId").value(1L));

    }

    @DisplayName("음원 게시물에 좋아요를 누른다.")
    @Test
    void likePost() throws Exception {
        // given
        Long memberId = 1L;
        Long postId = 1L;
        Number likeCount = 1;
        Boolean liked = true;

        PostPostLikeResDto response = postPostLikeResDto(likeCount, liked);
        given(postService.likePost(eq(memberId), eq(postId))).willReturn(response);

        // when // then
        MultiValueMap <String, String> params = new LinkedMultiValueMap<>();
        params.add("memberId", String.valueOf(memberId));
        mockMvc.perform(
                        post("/api/posts/" + postId + "/like")
                                .params(params)
                                .contentType(MediaType.APPLICATION_JSON)
                )
                .andDo(print())
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value("200"))
                .andExpect(jsonPath("$.message").value("success"))
                .andExpect(jsonPath("$.data.likeCount").value(1))
                .andExpect(jsonPath("$.data.liked").value(true));
    }

    private PostPostCreateReqDto postPostCreateReqDto(){
        return PostPostCreateReqDto.builder()
                .title("노래 제목")
                .musicPath("노래 주소")
                .albumImgPath("앨범 이미지 주소")
                .albumName("앨범 이름")
                .build();
    }

    private PostIdElement postIdElement(Long postId){
        return PostIdElement.of(postId);
    }

    private PostPostLikeResDto postPostLikeResDto(Number likeCount, Boolean liked) {
        return PostPostLikeResDto.of(likeCount, liked);
    }

}
