package com.soundie.post.repository;

import com.soundie.post.domain.Post;
import com.soundie.global.util.fixture.MemberFixture;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

class MemoryPostRepositoryTest {

    MemoryPostRepository postRepository = new MemoryPostRepository();

    @BeforeEach
    public void setUp() {
        postRepository.clearStore();
    }

    @DisplayName("음원 게시물 목록을 조회한다.")
    @Test
    public void findPosts() {
        // given
        Post post1 = createPost("노래 제목1", "노래 주소1", "앨범 이미지 주소1", "앨범 이름1");
        postRepository.save(post1);
        Post post2 = createPost("노래 제목2", "노래 주소2", "앨범 이미지 주소2", "앨범 이름2");
        postRepository.save(post2);

        // when
        List<Post> posts = postRepository.findPosts();

        // then
        assertThat(posts).hasSize(2)
                .extracting("id", "title", "musicPath", "albumImgPath", "albumName")
                .containsExactlyInAnyOrder(
                        tuple(1L,
                                "노래 제목1",
                                "노래 주소1",
                                "앨범 이미지 주소1",
                                "앨범 이름1"),
                        tuple(2L,
                                "노래 제목2",
                                "노래 주소2",
                                "앨범 이미지 주소2",
                                "앨범 이름2")
                );
    }

    @DisplayName("음원 게시물 Id로, 음원 게시물을 조회한다.")
    @Test
    public void findPost() {
        // given
        Post post1 = createPost("노래 제목1", "노래 주소1", "앨범 이미지 주소1", "앨범 이름1");
        postRepository.save(post1);

        // when
        Optional<Post> post = postRepository.findPostById(post1.getId());

        // then
        assertThat(post).isEqualTo(Optional.of(post1));
    }

    @DisplayName("음원 게시물이 하나도 없는 경우에는, null 을 반환한다.")
    @Test
    public void findPostWhenPostIsEmpty() {
        // when
        Optional<Post> post = postRepository.findPostById(null);

        // then
        assertThat(post).isEmpty();
    }

    @DisplayName("음원 게시물을 저장한다.")
    @Test
    public void save() {
        // given
        Post post1 = createPost("노래 제목1", "노래 주소1", "앨범 이미지 주소1", "앨범 이름1");

        // when
        Post post = postRepository.save(post1);

        // then
        assertThat(post).isEqualTo(post1);
    }

    private Post createPost(String title, String musicPath, String albumImgPath, String albumName) {
        return new Post(
                MemberFixture.createFirstMember().getId(),
                title,
                MemberFixture.createFirstMember().getName(),
                musicPath,
                albumImgPath,
                albumName
        );
    }
}
