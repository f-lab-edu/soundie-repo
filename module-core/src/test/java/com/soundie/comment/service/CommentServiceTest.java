package com.soundie.comment.service;


import com.soundie.comment.domain.Comment;
import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.repository.MemoryCommentRepository;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.global.util.fixture.CommentFixture;
import com.soundie.global.util.fixture.MemberFixture;
import com.soundie.global.util.fixture.PostFixture;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemoryMemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.repository.MemoryPostRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
class CommentServiceTest {

    @InjectMocks CommentService commentService;

    @Mock
    private MemoryCommentRepository commentRepository;

    @Mock
    private MemoryMemberRepository memberRepository;

    @Mock
    private MemoryPostRepository postRepository;

    @DisplayName("유효한 postId가 주어졌다면, 음원 게시물의 댓글 목록 조회가 성공합니다.")
    @Test
    void Given_PostId_When_ReadCommentList_Then_Success()  {
        // given
        Member member1 = MemberFixture.createFirstMember();
        Member member2 = MemberFixture.createSecondMember();

        Post post = PostFixture.createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));

        List<Comment> comments = List.of(
                CommentFixture.createFirstComment(member1, post),
                CommentFixture.createSecondComment(member2, post));
        given(commentRepository.findCommentsByPostId(post.getId())).willReturn(comments);

        Map<Long, Member> linkedHashMap = new LinkedHashMap<>();
        for (Comment comment : comments){
            if (comment.getMemberId().equals(member1.getId())){
                given(memberRepository.findMemberById(comment.getId())).willReturn(Optional.of(member1));
                linkedHashMap.put(comment.getId(), member1);
            } else if (comment.getMemberId().equals(member2.getId())) {
                given(memberRepository.findMemberById(comment.getId())).willReturn(Optional.of(member2));
                linkedHashMap.put(comment.getId(), member2);
            }
        }

        // when
        GetCommentResDto response = commentService.readCommentList(post.getId());

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(GetCommentResDto.of(comments, linkedHashMap));
    }

    @DisplayName("유효하지 않은 postId가 주어졌다면, 음원 게시물의 댓글 목록 조회가 실패합니다.")
    @Test
    void Given_InvalidPostId_When_ReadCommentList_Then_Fail()  {
        // given
        Long invalidPostId = 100L;
        given(postRepository.findPostById(invalidPostId)).willReturn(Optional.empty());

        // when // then
        assertThatThrownBy(() -> commentService.readCommentList(invalidPostId))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("음원 게시물을 찾을 수 없습니다.");
    }

    @DisplayName("로그인 시 유효한 memberId와 postId가 주어졌다면, 음원 게시물의 댓글 등록이 성공합니다.")
    @Test
    void Given_MemberId_When_CreateComment_Then_Success() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        given(postRepository.findPostById(post.getId())).willReturn(Optional.of(post));

        Comment comment = CommentFixture.createFirstComment(member, post);
        given(commentRepository.save(any(Comment.class))).willReturn(comment);

        PostCommentCreateReqDto postCommentCreateReqDto = new PostCommentCreateReqDto(
                "댓글 내용"
        );

        // when
        CommentIdElement response = commentService.createComment(
                member.getId(),
                post.getId(),
                postCommentCreateReqDto
        );

        // then
        assertThat(response).usingRecursiveComparison()
                .isEqualTo(CommentIdElement.of(comment.getId()));
    }

    @DisplayName("유효하지 않은 memberId가 주어졌다면, 음원 게시물의 댓글 등록이 실패합니다.")
    @Test
    void Given_InvalidMemberId_When_CreateComment_Then_Fail() {
        // given
        Long invalidMemberId = 100L;
        given(memberRepository.findMemberById(invalidMemberId)).willReturn(Optional.empty());
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        PostCommentCreateReqDto postCommentCreateReqDto = new PostCommentCreateReqDto(
                "댓글 내용"
        );

        // when // then
        assertThatThrownBy(() -> commentService.createComment(invalidMemberId, post.getId(), postCommentCreateReqDto))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("사용자를 찾을 수 없습니다.");
    }

    @DisplayName("유효하지 않은 postId가 주어졌다면, 음원 게시물의 댓글 등록이 실패합니다.")
    @Test
    void Given_InvalidPostId_When_CreateComment_Then_Fail() {
        // given
        Member member = MemberFixture.createFirstMember();
        given(memberRepository.findMemberById(member.getId())).willReturn(Optional.of(member));
        Long invalidPostId = 100L;
        given(postRepository.findPostById(invalidPostId)).willReturn(Optional.empty());

        PostCommentCreateReqDto postCommentCreateReqDto = new PostCommentCreateReqDto(
                "댓글 내용"
        );

        // when // then
        assertThatThrownBy(() -> commentService.createComment(member.getId(), invalidPostId, postCommentCreateReqDto))
                .isInstanceOf(NotFoundException.class)
                .hasMessage("음원 게시물을 찾을 수 없습니다.");
    }
}
