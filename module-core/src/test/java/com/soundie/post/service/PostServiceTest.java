package com.soundie.post.service;

import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemoryMemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.post.dto.*;
import com.soundie.post.global.util.fixture.MemberFixture;
import com.soundie.post.repository.MemoryPostLikeRepository;
import com.soundie.post.repository.MemoryPostRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static com.soundie.post.global.util.fixture.PostFixture.createFirstMemberHavingFirstPost;
import static com.soundie.post.global.util.fixture.PostFixture.createFirstMemberHavingSecondPost;
import static org.mockito.BDDMockito.*;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@ExtendWith(MockitoExtension.class)
class PostServiceTest {

    @InjectMocks
    private PostService postService;

    @Mock
    private MemoryMemberRepository memberRepository;

    @Mock
    private MemoryPostRepository postRepository;

    @Mock
    private MemoryPostLikeRepository postLikeRepository;

    @DisplayName("음원 게시물 목록 조회가 성공합니다.")
    @Test
    void When_ReadPostList_Then_Success()  {
        // given
        List<Post> postList = List.of(createFirstMemberHavingFirstPost(), createFirstMemberHavingSecondPost());
        given(postRepository.findPosts()).willReturn(postList);

        // when
        GetPostResDto response = postService.readPostList();

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(GetPostResDto.of(postList));
    }

    @DisplayName("로그인 시 유효한 postId와 memberId가 주어졌다면, 음원 게시물 조회가 성공합니다.")
    @Test
    void Given_PostIdAndMemberId_When_ReadPost_Then_Success() {
        // given
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));

        // when
        GetPostDetailResDto response = postService.readPost(member.getId(), post.getId());

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(GetPostDetailResDto.of(post, member));
    }

    @DisplayName("비 로그인 시 유효한 postId가 주어졌다면, 음원 게시물 조회가 성공합니다.")
    @Test
    void Given_PostId_When_ReadPost_Then_Success() {
        // given
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));

        // when
        GetPostDetailResDto response = postService.readPost(null, post.getId());

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(GetPostDetailResDto.of(post, null));
    }

    @DisplayName("유효하지 않은 postId가 주어졌다면, 음원 게시물 조회가 실패합니다.")
    @Test
    void Given_InvalidPostId_When_ReadPost_Then_Fail() {
        // given
        Long invalidPostId = 100L;
        given(postRepository.findPostById(invalidPostId)).willReturn(Optional.empty());

        // when // then
        assertThatThrownBy(() -> postService.readPost(null, invalidPostId))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("음원 게시물을 찾을 수 없습니다.");
    }

    @DisplayName("유효하지 않은 memberId가 주어졌다면, 음원 게시물 조회가 실패합니다.")
    @Test
    void Given_InvalidMemberId_When_ReadPost_Then_Fail() {
        // given
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));
        Long invalidMemberId = 100L;
        given(memberRepository.findMemberById(invalidMemberId)).willReturn(Optional.empty());

        // when // then
        assertThatThrownBy(() -> postService.readPost(invalidMemberId, post.getId()))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("사용자를 찾을 수 없습니다.");
    }

    @DisplayName("로그인 시 유효한 memberId가 주어졌다면, 음원 게시물 등록이 성공합니다.")
    @Test
    void Given_MemberId_When_CreatePost_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.save(any())).willReturn(post);

        PostPostCreateReqDto postPostCreateReqDto = new PostPostCreateReqDto(
                "노래 제목",
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );

        // when
        PostIdElement response = postService.createPost(member.getId(), postPostCreateReqDto);

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(PostIdElement.of(post.getId()));
    }

    @DisplayName("유효하지 않은 memberId가 주어졌다면, 음원 게시물 등록이 실패합니다.")
    @Test
    void Given_InvalidMemberId_When_CreatePost_Then_Fail() {
        // given
        Long invalidMemberId = 100L;
        given(memberRepository.findMemberById(invalidMemberId)).willReturn(Optional.empty());

        PostPostCreateReqDto postPostCreateReqDto = new PostPostCreateReqDto(
                "노래 제목",
                "노래 주소",
                "앨범 이미지 주소",
                "앨범 이름"
        );

        // when // then
        assertThatThrownBy(() -> postService.createPost(invalidMemberId, postPostCreateReqDto))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("사용자를 찾을 수 없습니다.");
    }

    @DisplayName("음원 게시물을 좋아요 하지 않았다면, 음원 게시물 좋아요 달기가 성공합니다.")
    @Test
    void Given_MemberIdAndPostId_When_SaveLikePost_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));
        given(postLikeRepository.findPostLikeByMemberIdAndPostId(member.getId(), post.getId())).willReturn(Optional.empty());
        Long likeCount = 0L;
        given(postLikeRepository.countPostLikesByPostId(post.getId())).willReturn(likeCount);

        // when
        PostPostLikeResDto response = postService.likePost(member.getId(), post.getId());

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(PostPostLikeResDto.of(likeCount + 1, true));
    }

    @DisplayName("음원 게시물을 좋아요를 했었다면, 음원 게시물 좋아요 취소가 성공합니다.")
    @Test
    void Given_MemberIdAndPostId_When_DeleteLikePost_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Post post = createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));
        PostLike postLike = new PostLike(member.getId(), post.getId());
        given(postLikeRepository.findPostLikeByMemberIdAndPostId(member.getId(), post.getId())).willReturn(Optional.of(postLike));
        Long likeCount = 1L;
        given(postLikeRepository.countPostLikesByPostId(post.getId())).willReturn(likeCount);

        // when
        PostPostLikeResDto response = postService.likePost(member.getId(), post.getId());

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(PostPostLikeResDto.of(likeCount - 1, false));
    }

    @DisplayName("유효하지 않은 memberId가 주어졌다면, 음원 게시물 좋아요 달기가 실패합니다.")
    @Test
    void Given_InvalidMemberId_When_SaveLikePost_Then_Fail() {
        // given
        Long invalidMemberId = 100L;
        given(memberRepository.findMemberById(invalidMemberId)).willReturn(Optional.empty());
        Post post = createFirstMemberHavingFirstPost();

        // when // then
        assertThatThrownBy(() -> postService.likePost(invalidMemberId, post.getId()))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("사용자를 찾을 수 없습니다.");
    }

    @DisplayName("유효하지 않은 postId가 주어졌다면, 음원 게시물 좋아요 달기가 실패합니다.")
    @Test
    void Given_InvalidPostId_When_SaveLikePost_Then_Fail() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Long invalidPostId = 100L;
        given(postRepository.findPostById(invalidPostId)).willReturn(Optional.empty());

        // when // then
        assertThatThrownBy(() -> postService.likePost(member.getId(), invalidPostId))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("음원 게시물을 찾을 수 없습니다.");
    }
}
