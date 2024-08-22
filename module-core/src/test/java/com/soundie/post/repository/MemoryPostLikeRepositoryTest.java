package com.soundie.post.repository;

import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.global.util.fixture.MemberFixture;
import com.soundie.global.util.fixture.PostFixture;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class MemoryPostLikeRepositoryTest {

    MemoryPostLikeRepository postLikeRepository = new MemoryPostLikeRepository();

    @BeforeEach
    public void setUp() {
        postLikeRepository.clearStore();
    }

    @DisplayName("회원 Id와 음원 게시물 Id로, 음원 게시물의 좋아요를 조회한다.")
    @Test
    public void findPostLikeByMemberIdAndPostId() {
        // given
        Member member = MemberFixture.createFirstMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        PostLike postLike1 = createPostLike(member.getId(), post.getId());
        postLikeRepository.save(postLike1);

        // when
        Optional<PostLike> postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(
                member.getId(),
                post.getId()
        );

        // then
        assertThat(postLike).isEqualTo(Optional.of(postLike1));
    }

    @DisplayName("음원 게시물의 좋아요가 하나도 없는 경우에는, null 을 반환한다.")
    @Test
    public void findPostLikeWhenPostLikeIsEmpty() {
        // given
        Member member = MemberFixture.createFirstMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        // when
        Optional<PostLike> postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(
                member.getId(),
                post.getId()
        );

        // then
        assertThat(postLike).isEmpty();
    }

    @DisplayName("음원 게시물의 좋아요 개수를 조회한다.")
    @Test
    public void countPostLikesByPostId() {
        // given
        Member member1 = MemberFixture.createFirstMember();
        Member member2 = MemberFixture.createSecondMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        PostLike postLike1 = createPostLike(member1.getId(), post.getId());
        postLikeRepository.save(postLike1);
        PostLike postLike2 = createPostLike(member2.getId(), post.getId());
        postLikeRepository.save(postLike2);

        // when
        Long likeCount = postLikeRepository.countPostLikesByPostId(
                post.getId()
        );

        // then
        assertThat(likeCount).isEqualTo(2);
    }

    @DisplayName("음원 게시물의 좋아요를 저장한다.")
    @Test
    public void save() {
        // given
        Member member = MemberFixture.createFirstMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();
        PostLike postLike1 = createPostLike(member.getId(), post.getId());

        // when
        PostLike postLike = postLikeRepository.save(postLike1);

        // then
        assertThat(postLike).isEqualTo(postLike1);
    }

    @DisplayName("음원 게시물의 좋아요를 삭제한다.")
    @Test
    public void delete() {
        // given
        Member member = MemberFixture.createFirstMember();
        Post post = PostFixture.createFirstMemberHavingFirstPost();

        PostLike postLike1 = createPostLike(member.getId(), post.getId());
        postLikeRepository.save(postLike1);

        // when
        postLikeRepository.delete(postLike1);

        // then
        Optional<PostLike> postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(
                member.getId(),
                post.getId()
        );
        assertThat(postLike).isNotPresent();
    }

    private PostLike createPostLike(Long memberId, Long postId) {
        return new PostLike(memberId, postId);
    }
}
