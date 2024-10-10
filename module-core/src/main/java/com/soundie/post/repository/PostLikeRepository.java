package com.soundie.post.repository;

import com.soundie.post.domain.PostLike;

import java.util.List;
import java.util.Optional;

public interface PostLikeRepository {

    List<PostLike> findPostLikes();

    Optional<PostLike> findPostLikeByMemberIdAndPostId(Long memberId, Long postId);

    Number countPostLikesByPostId(Long postId);

    PostLike save(PostLike postLike);

    List<PostLike> saveAll(List<PostLike> postLikes);

    void delete(PostLike postLike);
}
