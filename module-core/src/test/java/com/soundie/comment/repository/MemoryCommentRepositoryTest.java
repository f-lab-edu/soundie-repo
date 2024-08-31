package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;
import com.soundie.global.util.fixture.MemberFixture;
import com.soundie.global.util.fixture.PostFixture;
import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

class MemoryCommentRepositoryTest {

    MemoryCommentRepository commentRepository = new MemoryCommentRepository();

    @BeforeEach
    public void setUp() {
        commentRepository.clearStore();
    }

    @DisplayName("음원 게시물의 댓글 목록을 조회한다.")
    @Test
    public void findCommentsByPostId() {
        // given
        Member member1 = MemberFixture.createFirstMember();
        Member member2 = MemberFixture.createSecondMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        Comment comment1 = createComment(member1.getId(), post.getId(), "댓글 내용1");
        commentRepository.save(comment1);
        Comment comment2 = createComment(member2.getId(), post.getId(), "댓글 내용2");
        commentRepository.save(comment2);

        // when
        List<Comment> comments = commentRepository.findCommentsByPostId(post.getId());

        // then
        assertThat(comments).hasSize(2)
                .extracting("id", "content")
                .containsExactlyInAnyOrder(
                        tuple(1L,
                                "댓글 내용1"),
                        tuple(2L,
                                "댓글 내용2")
                );
    }

    @DisplayName("음원 게시물의 댓글을 저장한다.")
    @Test
    public void save() {
        // given
        Member member = MemberFixture.createFirstMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        Comment comment1 = createComment(member.getId(), post.getId(), "댓글 내용1");

        // when
        Comment comment = commentRepository.save(comment1);

        // then
        assertThat(comment).isEqualTo(comment1);
    }

    @DisplayName("음원 게시물의 댓글 수를 조회한다.")
    @Test
    public void countCommentsByPostId() {
        // given
        Member member1 = MemberFixture.createFirstMember();
        Member member2 = MemberFixture.createSecondMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        Comment comment1 = createComment(member1.getId(), post.getId(), "댓글 내용1");
        commentRepository.save(comment1);
        Comment comment2 = createComment(member2.getId(), post.getId(), "댓글 내용2");
        commentRepository.save(comment2);

        // when
        Long commentCount = commentRepository.countCommentsByPostId(
                post.getId()
        );

        // then
        assertThat(commentCount).isEqualTo(2);
    }

    private Comment createComment(Long memberId, Long postId, String content) {
        return new Comment(memberId, postId, content);
    }
}
