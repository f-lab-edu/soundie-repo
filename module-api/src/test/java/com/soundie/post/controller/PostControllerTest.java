package com.soundie.post.controller;

import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import com.soundie.post.dto.GetPostDetailResDto;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.service.PostService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = PostController.class)
class PostControllerTest {

    @Autowired
    private MockMvc mockMvc;

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
}
