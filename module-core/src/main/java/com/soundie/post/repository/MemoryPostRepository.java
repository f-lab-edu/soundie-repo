package com.soundie.post.repository;

import com.soundie.post.domain.Post;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class MemoryPostRepository implements PostRepository {

    private final Map<Long, Post> store = new ConcurrentHashMap<>();
    private AtomicLong sequence = new AtomicLong(0L);

    /*
    * 음원 게시물 목록 조회
    * */
    @Override
    public List<Post> findPosts() {
        return new ArrayList<>(store.values());
    }

    
    @Override
    public List<Post> findPostsOrderByIdDesc(Integer size) {
        // 구현 필요
        return null;
    }

    @Override
    public List<Post> findPostsByIdLessThanOrderByIdDesc(Long postId, Integer size) {
        // 구현 필요
        return null;
    }

    /*
     * 음원 게시물 Id로, 음원 게시물 조회
     * */
    @Override
    public Optional<Post> findPostById(Long postId) {
        return Optional.ofNullable(store.get(postId));
    }

    /*
    * 음원 게시물 저장
    * */
    @Override
    public Post save(Post post){
        post.setId(sequence.incrementAndGet());
        store.put(post.getId(), post);

        return post;
    }

    public void clearStore() {
        store.clear();
    }
}
