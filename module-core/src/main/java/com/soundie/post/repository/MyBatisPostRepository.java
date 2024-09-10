package com.soundie.post.repository;

import com.soundie.post.domain.Post;
import com.soundie.post.mapper.PostMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisPostRepository implements PostRepository {

    private final PostMapper postMapper;

    /*
     * 음원 게시물 목록 조회
     * */
    @Override
    public List<Post> findPosts() {
        return postMapper.findPosts();
    }

    @Override
    public List<Post> findPostsByOrderByIdDescCreatedAtDesc(Integer size) {
        return postMapper.findPostsByOrderByIdDescCreatedAtDesc(size);
    }

    @Override
    public List<Post> findPostsByIdLessThanOrderByIdDescCreatedAtDesc(Long postId, Integer size) {
        return postMapper.findPostsByIdLessThanOrderByIdDescCreatedAtDesc(postId, size);
    }

    /*
     * 음원 게시물 Id로, 음원 게시물 조회
     * */
    @Override
    public Optional<Post> findPostById(Long postId) {
        return postMapper.findPostById(postId);
    }

    /*
     * 음원 게시물 저장
     * */
    @Override
    public Post save(Post post) {
        postMapper.save(post);
        return post;
    }
}
