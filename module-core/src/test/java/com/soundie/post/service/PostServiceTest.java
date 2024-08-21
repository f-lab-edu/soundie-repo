package com.soundie.post.service;

import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.dto.GetPostDetailResDto;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.dto.PostIdElement;
import com.soundie.post.dto.PostPostCreateReqDto;
import com.soundie.post.global.util.fixture.MemberFixture;
import com.soundie.post.global.util.fixture.PostFixture;
import com.soundie.post.repository.PostRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@SpringBootTest
class PostServiceTest {

    @Autowired
    private PostService postService;

    @Autowired
    private MemberRepository memberRepository;

    @Autowired
    private PostRepository postRepository;

    @AfterEach
    void tearDown() {
        postRepository.clearStore();
        memberRepository.clearStore();
    }

    @DisplayName("음원 게시물 목록 조회가 성공합니다.")
    @Test
    void When_readPostList_Then_Success()  {
        // given
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        postRepository.save(post);

        // when
        GetPostResDto getPostResDto = postService.readPostList();

        // then
        assertThat(getPostResDto.getPosts()).hasSize(1);
    }

    @DisplayName("로그인 시 유효한 postId와 memberId가 주어졌다면, 음원 게시물 조회가 성공합니다.")
    @Test
    void Given_MemberIdAndPostId_When_readPost_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        memberRepository.save(member);
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        postRepository.save(post);

        // when
        GetPostDetailResDto getPostDetailResDto = postService.readPost(member.getId(), post.getId());

        // then
        assertThat(getPostDetailResDto.getPost().getPostId()).isEqualTo(post.getId());
    }

    @DisplayName("비 로그인 시, 유효한 postId가 주어졌다면, 음원 게시물 조회가 성공합니다.")
    @Test
    void Given_PostId_When_readPost_Then_Success() {
        // given
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        postRepository.save(post);

        // when
        GetPostDetailResDto getPostDetailResDto = postService.readPost(null, post.getId());

        // then
        assertThat(getPostDetailResDto.getPost().getPostId()).isEqualTo(post.getId());
    }

    @DisplayName("유효하지 않은 postId가 주어졌다면, 음원 게시물 조회가 실패합니다.")
    @Test
    void Given_InvalidPostId_When_readPost_Then_Fail() {
        // given
        Member member = MemberFixture.createFirstMember();
        memberRepository.save(member);
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        postRepository.save(post);


        // when // then
        Long invalidPostId = 100L;
        assertThatThrownBy(() -> postService.readPost(member.getId(), invalidPostId))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("음원 게시물을 찾을 수 없습니다.");
    }

    @DisplayName("유효하지 않은 memberId가 주어졌다면, 음원 게시물 조회가 실패합니다.")
    @Test
    void Given_InvalidMemberId_When_readPost_Then_Fail() {
        // given
        Member member = MemberFixture.createFirstMember();
        memberRepository.save(member);
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        postRepository.save(post);


        // when // then
        Long invalidMemberId = 100L;
        assertThatThrownBy(() -> postService.readPost(invalidMemberId, post.getId()))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("사용자를 찾을 수 없습니다.");
    }

    @DisplayName("로그인 시 유효한 memberId가 주어졌다면, 음원 게시물 등록이 성공합니다.")
    @Test
    void Given_MemberId_When_createPost_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        memberRepository.save(member);

        PostPostCreateReqDto postPostCreateReqDto = PostPostCreateReqDto.builder()
                .title("노래 제목")
                .musicPath("노래 주소")
                .albumImgPath("앨범 이미지 주소")
                .albumName("앨범 이름")
                .build();

        // when
        PostIdElement postIdElement = postService.createPost(member.getId(), postPostCreateReqDto);

        // then
        assertThat(postIdElement.getPostId()).isEqualTo(1L);
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
