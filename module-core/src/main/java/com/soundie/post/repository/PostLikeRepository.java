package com.soundie.post.repository;

import com.soundie.post.domain.PostLike;

import java.util.List;
import java.util.Optional;

public interface PostLikeRepository {

    List<PostLike> findPostLikes();

    Optional<PostLike> findPostLikeByMemberIdAndPostId(Long memberId, Long postId);

    Long countPostLikesByPostId(Long postId);

    PostLike save(PostLike postLike);

    void delete(PostLike postLike);
}
