package com.soundie.post.repository;

import com.soundie.post.domain.Post;

import java.util.List;
import java.util.Optional;

public interface PostRepository {

    List<Post> findPosts();

    List<Post> findPostsByOrderByIdDescCreatedAtDesc(Integer size);

    List<Post> findPostsByIdLessThanOrderByIdDescCreatedAtDesc(Long postId, Integer size);

    Optional<Post> findPostById(Long postId);

    Post save(Post post);
}
