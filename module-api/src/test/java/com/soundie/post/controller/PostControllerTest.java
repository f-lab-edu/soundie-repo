package com.soundie.post.controller;

import com.soundie.member.domain.Member;
import com.soundie.post.ControllerTestSupport;
import com.soundie.post.domain.Post;
import com.soundie.post.dto.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.util.List;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = PostController.class)
class PostControllerTest extends ControllerTestSupport {

    @DisplayName("음원 게시물 목록을 조회한다.")
    @Test
    void readPostList() throws Exception {
        // given
        GetPostResDto response = GetPostResDto.of(List.of());
        given(postService.readPostList()).willReturn(response);

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
        Long postId = 1L;

        GetPostDetailResDto response = getPostDetailResDto(memberId, postId);
        given(postService.readPost(any(), any())).willReturn(response);

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
        Long postId = 1L;

        PostPostCreateReqDto request = postPostCreateReqDto();
        PostIdElement response = postIdElement(postId);
        given(postService.createPost(anyLong(), any(PostPostCreateReqDto.class))).willReturn(response);

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
        given(postService.likePost(anyLong(), anyLong())).willReturn(response);

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

    private GetPostDetailResDto getPostDetailResDto(Long memberId, Long postId) {
        Member member = new Member("정원석");
        member.setId(memberId);
        Post post = new Post(
                memberId,
                "노래 제목",
                member.getName(),
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );
        post.setId(postId);

        return GetPostDetailResDto.of(post, member);
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
