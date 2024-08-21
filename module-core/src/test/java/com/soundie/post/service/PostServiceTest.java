package com.soundie.post.service;

import com.soundie.post.domain.Post;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.repository.PostRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static com.soundie.post.global.util.fixture.PostFixture.POST_FIXTURE1;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class PostServiceTest {

    @Autowired
    private PostService postService;

    @Autowired
    private PostRepository postRepository;

    @AfterEach
    void tearDown() {
        postRepository.clearStore();
    }

    @DisplayName("음원 게시물 목록 조회가 성공합니다.")
    @Test
    void When_readPostList_Then_Success()  {
        // given
        Post post = POST_FIXTURE1;
        postRepository.save(post);

        // when
        GetPostResDto getPostResDto = postService.readPostList();

        // then
        assertThat(getPostResDto.getPosts()).hasSize(1);
        assertThat(getPostResDto.getPosts().iterator().next().getPostId()).isEqualTo(1L);
    }

    @DisplayName("음원 게시물을 조회한다.")
    @Test
    void readPost() {
        // given

        // when

        // then
    }

    @DisplayName("음원 게시물을 등록한다.")
    @Test
    void createPost() {
        // given

        // when

        // then
    }

    @DisplayName("음원 게시물에 좋아요를 누른다.")
    @Test
    void likePost() {
        // given

        // when

        // then
    }
}
