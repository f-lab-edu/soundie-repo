package com.soundie.post.repository;

import com.soundie.post.domain.Post;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Repository
public class PostRepository {

    private static final Map<Long, Post> store = new HashMap<>(); //static
    private static long sequence = 0L; //static

    /*
    * 음원 게시물 목록 조회
    * */
    public List<Post> findPosts() {
        return new ArrayList<>(store.values());
    }

    /*
     * 음원 게시물 Id로, 음원 게시물 조회
     * */
    public Post findPostById(Long postId) {
        return store.get(postId);
    }

    /*
    * 음원 게시물 저장
    * */
    public Post save(Post post){
        post.setId(++sequence);
        store.put(post.getId(), post);

        return post;
    }
}