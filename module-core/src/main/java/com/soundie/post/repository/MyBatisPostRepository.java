package com.soundie.post.repository;

import com.soundie.global.common.util.CacheNames;
import com.soundie.post.domain.Post;
import com.soundie.post.mapper.PostMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
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
    public List<Post> findPostsOrderByIdDesc(Integer size) {
        return postMapper.findPostsOrderByIdDesc(size);
    }

    @Override
    public List<Post> findPostsByIdLessThanOrderByIdDesc(Long postId, Integer size) {
        return postMapper.findPostsByIdLessThanOrderByIdDesc(postId, size);
    }

    /*
     * 음원 게시물 Id로, 음원 게시물 조회
     * */
    @Override
    @Cacheable(cacheNames = CacheNames.POST, key = "'postId_' + #postId")
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

    /*
    * 음원 게시물 삭제
    * */
    @Override
    public void delete(Post post) {
        postMapper.delete(post);
    }
}
